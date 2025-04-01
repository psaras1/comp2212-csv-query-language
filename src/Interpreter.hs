{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Interpreter (
    interpretQuery,
    runQueryFile,
    runQueryOnFiles
) where

import CSV
import Parser
import Lexer
import Data.List (nub, intercalate, isPrefixOf)
import Data.Maybe (fromMaybe)
import System.Directory (doesFileExist)
import Control.Exception (IOException, try)
import qualified Data.Map as Map

-- | Main function to interpret a query and execute it on given CSV files
interpretQuery :: String -> IO (Either String CSV)
interpretQuery queryStr = do
    -- The list of available files will be discovered dynamically
    let tokens = lexer queryStr
    case tokens of
        [] -> return $ Left "Empty query"
        _ -> do
            let queries = parseQueries tokens
            runQueries queries Map.empty

-- | Run a list of queries sequentially
runQueries :: [QueryExpr] -> Map.Map String CSV -> IO (Either String CSV)
runQueries [] _ = return $ Left "No queries to execute"
runQueries [query] tables = do
    -- For the last query, return its result
    result <- processQueryWithState query tables
    case result of
        Left err -> return $ Left err
        Right (csv, _) -> return $ Right (sortCSV csv)  -- Extract just the CSV and sort it
runQueries (query:rest) tables = do
    -- Process the current query
    result <- processQueryWithState query tables
    case result of
        Left err -> return $ Left err
        Right (_, newTables) ->
            -- Process the remaining queries with updated tables
            runQueries rest newTables

-- | Process a single query with state
processQueryWithState :: QueryExpr -> Map.Map String CSV -> IO (Either String (CSV, Map.Map String CSV))
processQueryWithState query tables = do
    -- Extract table names from the query
    let tableNames = getTableNames query
    -- Filter out table names that already exist in the temporary tables
    let filesToLoad = filter (\name -> not (Map.member name tables)) tableNames
    -- Load only the CSV files that don't exist as temporary tables
    fileTables <- loadCSVFiles filesToLoad
    case fileTables of
        Left err ->
            -- Special case: if the error is about a table not found, check if it might be a typo
            if "File not found" `isPrefixOf` err then
                -- If there are temporary tables with similar names, suggest them
                let missingTable = head (words (drop 16 err))
                    similarTables = filter (\name -> length name > 0 &&
                                          head name == head missingTable)
                                          (Map.keys tables)
                    suggestion = if null similarTables
                                 then ""
                                 else ". Did you mean one of these: " ++
                                      intercalate ", " similarTables ++ "?"
                in return $ Left (err ++ suggestion)
            else return $ Left err
        Right fileMap -> do
            -- Combine file data with any temporary tables
            -- temporary tables take precedence over files
            let allTables = Map.union tables fileMap
            -- Evaluate the query
            case evalQueryWithState query allTables of
                Left err -> return $ Left (err ++ " in query: " ++ show query)
                Right result -> return $ Right result

-- | Evaluate a query on loaded CSV data and return the updated state
evalQueryWithState :: QueryExpr -> Map.Map String CSV -> Either String (CSV, Map.Map String CSV)
evalQueryWithState query tables = case query of
    CreateTable name subQuery -> do
        result <- evalQuery subQuery tables
        -- Store the result in the tables map
        let newTables = Map.insert name result tables
        Right (result, newTables)
    -- For other queries, just evaluate without changing tables
    _ -> do
        result <- evalQuery query tables
        Right (result, tables)

-- | Extract table names from a query
getTableNames :: QueryExpr -> [String]
getTableNames query = case query of
    Select _ table _ -> getTablesFromExpr table
    SelectDistinct _ table _ -> getTablesFromExpr table
    CartesianProduct tables -> concatMap getTablesFromExpr tables
    Permute _ table -> getTablesFromExpr table
    Drop _ table -> getTablesFromExpr table
    PermuteWhere _ table _ -> getTablesFromExpr table
    Exists _ table -> getTablesFromExpr table
    CopyWithConstant _ _ table -> getTablesFromExpr table
    LeftMerge table1 _ table2 -> getTablesFromExpr table1 ++ getTablesFromExpr table2
    Project _ table -> getTablesFromExpr table
    ProjectGroupBy _ table _ -> getTablesFromExpr table
    RenameColumn _ _ table -> getTablesFromExpr table
    CreateTable _ subQuery -> getTableNames subQuery
    Union query1 query2 -> getTableNames query1 ++ getTableNames query2

-- Helper to extract table names from table expressions
getTablesFromExpr :: TableExpr -> [String]
getTablesFromExpr expr = case expr of
    Table name -> [name]
    SubQuery query -> getTableNames query
    Join table1 table2 _ -> getTablesFromExpr table1 ++ getTablesFromExpr table2

-- | Load multiple CSV files and return them as a map
loadCSVFiles :: [String] -> IO (Either String (Map.Map String CSV))
loadCSVFiles tableNames = do
    results <- mapM (\name -> loadCSVFile name >>= \res -> return (name, res)) tableNames
    if any (isLeft . snd) results
        then return $ Left $ fromMaybe "Error loading CSV files" $ findFirstError results
        else return $ Right $ Map.fromList [(name, csv) | (name, Right csv) <- results]
  where
    isLeft (Left _) = True
    isLeft _ = False

    findFirstError results =
        let errors = [(name, err) | (name, Left err) <- results]
        in if null errors then Nothing else Just $ "Error loading " ++ fst (head errors) ++ ": " ++ snd (head errors)

-- | Load a single CSV file
loadCSVFile :: String -> IO (Either String CSV)
loadCSVFile name = do
    let filePath = name ++ ".csv"
    fileExists <- doesFileExist filePath
    if fileExists
        then do
            result <- try (readFile filePath) :: IO (Either IOException String)
            case result of
                Left e -> return $ Left $ "Error reading file: " ++ show e
                Right content -> return $ Right $ parseCSV content
        else return $ Left $ "File not found: " ++ filePath

-- | Evaluate a query on loaded CSV data
evalQuery :: QueryExpr -> Map.Map String CSV -> Either String CSV
evalQuery query tables = case query of
    Select cols table whereClause -> evalSelect cols table whereClause tables
    SelectDistinct cols table whereClause -> evalSelectDistinct cols table whereClause tables
    CartesianProduct tableList -> evalCartesianProduct tableList tables
    Permute cols table -> evalPermute cols table tables
    Drop cols table -> evalDrop cols table tables
    PermuteWhere cols table cond -> evalPermuteWhere cols table cond tables
    Exists colIdx table -> evalExists colIdx table tables
    CopyWithConstant colIdx constVal table -> evalCopyWithConstant colIdx constVal table tables
    LeftMerge table1 colIdx table2 -> evalLeftMerge table1 colIdx table2 tables
    Project cols table -> evalProject cols table tables
    ProjectGroupBy cols table groupCols -> evalProjectGroupBy cols table groupCols tables
    RenameColumn colIdx newName table -> evalRenameColumn colIdx newName table tables
    CreateTable _ subQuery -> do
        result <- evalQuery subQuery tables
        Right result  -- Return the result of the subquery
    Union query1 query2 -> evalUnion query1 query2 tables




-- | Evaluate a UNION operation
evalUnion :: QueryExpr -> QueryExpr -> Map.Map String CSV -> Either String CSV
evalUnion query1 query2 tables = do
    result1 <- evalQuery query1 tables
    result2 <- evalQuery query2 tables

    -- Check if the number of columns match
    if null result1 || null result2
        then Right $ result1 ++ result2  -- If either is empty, just concatenate
        else if length (head result1) /= length (head result2)
            then Left "UNION operations require the same number of columns"
            else Right $ nub (result1 ++ result2)  -- Remove duplicates

-- | Evaluate a SELECT query
evalSelect :: ColumnList -> TableExpr -> Maybe Condition -> Map.Map String CSV -> Either String CSV
evalSelect cols table whereClause tables = do
    tableData <- evalTableExpr table tables
    let filteredData = case whereClause of
            Nothing -> tableData
            Just cond -> filter (rowMatchesCondition cond tableData) tableData
    columnResolutions <- resolveColumnList cols tableData
    return $ projectColumns columnResolutions tableData filteredData

-- | Evaluate a SELECT DISTINCT query (removes duplicates)
evalSelectDistinct :: ColumnList -> TableExpr -> Maybe Condition -> Map.Map String CSV -> Either String CSV
evalSelectDistinct cols table whereClause tables = do
    tableData <- evalTableExpr table tables
    let filteredData = case whereClause of
            Nothing -> tableData
            Just cond -> filter (rowMatchesCondition cond tableData) tableData
    columnResolutions <- resolveColumnList cols tableData
    let result = projectColumns columnResolutions tableData filteredData
    return $ nub result

-- | Evaluate a CARTESIAN PRODUCT operation
evalCartesianProduct :: [TableExpr] -> Map.Map String CSV -> Either String CSV
evalCartesianProduct [] _ = Right []
evalCartesianProduct [table] tables = evalTableExpr table tables
evalCartesianProduct (table1:table2:rest) tables = do
    csv1 <- evalTableExpr table1 tables
    -- If there are more tables, first compute the product of table2 and the rest
    csv2 <- if null rest
               then evalTableExpr table2 tables
               else evalCartesianProduct (table2:rest) tables
    -- Handle empty CSV cases
    if null csv1 || null csv2
        then Right []
        else Right $ cartesianProduct csv1 csv2
  where
    -- Perform cartesian product of two CSV datasets
    cartesianProduct :: CSV -> CSV -> CSV
    cartesianProduct csv1 csv2 =
        [row1 ++ row2 | row1 <- csv1, row2 <- csv2]

-- | Evaluate a PERMUTE operation
evalPermute :: [ColIndex] -> TableExpr -> Map.Map String CSV -> Either String CSV
evalPermute colIndices table tables = do
    tableData <- evalTableExpr table tables
    -- Extract columns according to the specified indices
    let columnIndices = map (resolveColIndex tableData []) colIndices
    -- Guard against invalid column indices
    if any (< 0) columnIndices || any (>= length (head tableData)) columnIndices
        then Left "Column index out of bounds"
        else let permuteRow row = map (\idx -> row !! idx) columnIndices
             in Right $ map permuteRow tableData

-- | Evaluate a DROP operation
evalDrop :: [ColIndex] -> TableExpr -> Map.Map String CSV -> Either String CSV
evalDrop colIndices table tables = do
    tableData <- evalTableExpr table tables
    -- Extract columns to drop
    let columnIndices = map (resolveColIndex tableData []) colIndices
    -- Guard against invalid column indices
    if any (< 0) columnIndices || any (>= length (head tableData)) columnIndices
        then Left "Column index out of bounds"
        else let dropColumns row =
                   [val | (val, idx) <- zip row [0..], idx `notElem` columnIndices]
             in Right $ map dropColumns tableData

-- | Evaluate a PERMUTE with WHERE condition
evalPermuteWhere :: [ColIndex] -> TableExpr -> Condition -> Map.Map String CSV -> Either String CSV
evalPermuteWhere colIndices table cond tables = do
    tableData <- evalTableExpr table tables
    -- Filter rows based on the condition
    let filteredData = filter (rowMatchesCondition cond tableData) tableData
    -- Then permute the columns
    evalPermute colIndices (SubQuery (Select AllColumns (Table "temp") Nothing))
                (Map.singleton "temp" filteredData)

-- | Evaluate an EXISTS check
evalExists :: ColIndex -> TableExpr -> Map.Map String CSV -> Either String CSV
evalExists colIdx table tables = do
    tableData <- evalTableExpr table tables
    let colIndex = resolveColIndex tableData [] colIdx
    -- Check if the column index is valid
    if colIndex < 0 || colIndex >= length (head tableData)
        then Left "Column index out of bounds"
        else let filterEmptyCol row = row !! colIndex /= ""
             in Right $ filter filterEmptyCol tableData

-- | Evaluate a COPY with CONSTANT operation
evalCopyWithConstant :: ColIndex -> String -> TableExpr -> Map.Map String CSV -> Either String CSV
evalCopyWithConstant colIdx constVal table tables = do
    tableData <- evalTableExpr table tables
    let colIndex = resolveColIndex tableData [] colIdx
    -- Check if the column index is valid
    if colIndex < 0 || colIndex >= length (head tableData)
        then Left "Column index out of bounds"
        else let copyAndAdd row = [row !! colIndex, constVal, row !! colIndex]
             in Right $ map copyAndAdd tableData

-- | Evaluate a LEFT MERGE operation
evalLeftMerge :: TableExpr -> ColIndex -> TableExpr -> Map.Map String CSV -> Either String CSV
evalLeftMerge table1 colIdx table2 tables = do
    csv1 <- evalTableExpr table1 tables
    csv2 <- evalTableExpr table2 tables

    -- Ensure both tables have data
    if null csv1 || null csv2
        then Right []
        else do
            let joinColIndex = resolveColIndex csv1 [] colIdx

            -- Group rows from table2 by the join column value
            let table2ByJoinCol = groupByColumn joinColIndex csv2

            -- Merge a row from table1 with a matching row from table2
            let mergeRow row1 row2 =
                    let mergeCol idx =
                            if idx < length row1 && row1 !! idx == ""
                                then if idx < length row2 then row2 !! idx else ""
                                else row1 !! idx
                    in [mergeCol i | i <- [0..max (length row1 - 1) (length row2 - 1)]]

            let mergeRows row1 =
                    -- Look for matching rows in table2
                    case Map.lookup (row1 !! joinColIndex) table2ByJoinCol of
                        Just matchingRows ->
                            -- For each matching row, merge with row1
                            [mergeRow row1 matchingRow | matchingRow <- matchingRows]
                        Nothing -> [] -- Nothing if no match 

            -- Apply the merge operation
            let result = concatMap mergeRows csv1

            Right result

-- Helper function to group rows by a specific column
groupByColumn :: Int -> CSV -> Map.Map String [Row]
groupByColumn colIndex = foldl (\acc row ->
        let key = if colIndex < length row then row !! colIndex else ""
        in Map.insertWith (++) key [row] acc
    ) Map.empty

-- | Evaluate a PROJECT operation
-- Add this to Interpreter.hs
evalProject :: [ColIndex] -> TableExpr -> Map.Map String CSV -> Either String CSV
evalProject colIndices table tables = do
    tableData <- evalTableExpr table tables
    -- Handle empty tables
    if null tableData
        then Right []
        else do
            -- Check if there are any rows in the table
            let firstRow = head tableData
            let numCols = length firstRow

            -- Convert ColIndex to actual column indices
            let columnIndices = map (resolveColIndex tableData []) colIndices

            -- For Debugging
            let indexInfo = "Requested indices: " ++ show columnIndices ++
                           ", Available columns: " ++ show numCols ++
                           ", First row: " ++ show firstRow

            -- Guard against invalid column indices
            if any (< 0) columnIndices || any (>= numCols) columnIndices
                then Left $ "Column index out of bounds. " ++ indexInfo
                else let projectRow row = [row !! idx | idx <- columnIndices]
                     in Right $ map projectRow tableData

-- | Evaluate a PROJECT GROUP BY operation
evalProjectGroupBy :: [ColIndex] -> TableExpr -> [ColIndex] -> Map.Map String CSV -> Either String CSV
evalProjectGroupBy projCols table groupCols tables = do
    tableData <- evalTableExpr table tables
    -- Convert ColIndex to actual column indices
    let projIndices = map (resolveColIndex tableData []) projCols
        groupIndices = map (resolveColIndex tableData []) groupCols

    -- Group rows by the values in the groupBy columns
    let groupedData = groupByColumns groupIndices tableData

    -- For each group, project the specified columns
    let result = map (\group ->
                    let rep = head group -- Take the first row as representative
                    in [rep !! idx | idx <- projIndices]
                ) groupedData

    Right result

-- | Group rows by multiple column values
groupByColumns :: [Int] -> CSV -> [CSV]
groupByColumns colIndices csv =
    let groups = Map.fromListWith (++)
                [(tuple row, [row]) | row <- csv]
     in Map.elems groups
  where
    tuple row = [row !! idx | idx <- colIndices, idx < length row]

-- | Evaluate a RENAME COLUMN operation
evalRenameColumn :: ColIndex -> String -> TableExpr -> Map.Map String CSV -> Either String CSV
evalRenameColumn _ _ table tables = do
    -- We're just renaming for documentation, doesn't affect the data processing
    evalTableExpr table tables

-- | Evaluate a table expression
evalTableExpr :: TableExpr -> Map.Map String CSV -> Either String CSV
evalTableExpr table tables = case table of
    Table name ->
        case Map.lookup name tables of
            Just csv -> Right csv
            Nothing -> Left $ "Table not found: " ++ name
    SubQuery query -> evalQuery query tables
    Join table1 table2 cond -> evalJoin table1 table2 cond tables

-- | Evaluate a JOIN operation
evalJoin :: TableExpr -> TableExpr -> Condition -> Map.Map String CSV -> Either String CSV
evalJoin table1 table2 cond tables = do
    csv1 <- evalTableExpr table1 tables
    csv2 <- evalTableExpr table2 tables

    if null csv1 || null csv2
        then Right []
        else do
            let combined = cartesianProduct csv1 csv2
                -- Filter the combined rows based on the join condition
                result = filter (rowMatchesJoinCondition cond csv1 csv2) combined

            Right result
  where
    cartesianProduct csv1 csv2 =
        [row1 ++ row2 | row1 <- csv1, row2 <- csv2]

-- | Check if a row matches a join condition
rowMatchesJoinCondition :: Condition -> CSV -> CSV -> Row -> Bool
rowMatchesJoinCondition cond csv1 csv2 row =
    let combinedData = csv1 ++ csv2
    in evalCondition cond combinedData row

-- | Check if a row matches a condition
rowMatchesCondition :: Condition -> CSV -> Row -> Bool
rowMatchesCondition = evalCondition

-- | Evaluate a condition against a row
evalCondition :: Condition -> CSV -> Row -> Bool
evalCondition cond csv row = case cond of
    And cond1 cond2 -> evalCondition cond1 csv row && evalCondition cond2 csv row
    Or cond1 cond2 -> evalCondition cond1 csv row || evalCondition cond2 csv row
    Equals expr1 expr2 -> evalExpr expr1 csv row == evalExpr expr2 csv row
    NotEquals expr1 expr2 -> evalExpr expr1 csv row /= evalExpr expr2 csv row
    LessThan expr1 expr2 -> evalExpr expr1 csv row < evalExpr expr2 csv row
    GreaterThan expr1 expr2 -> evalExpr expr1 csv row > evalExpr expr2 csv row
    LessEquals expr1 expr2 -> evalExpr expr1 csv row <= evalExpr expr2 csv row
    GreaterEquals expr1 expr2 -> evalExpr expr1 csv row >= evalExpr expr2 csv row
    IsNull expr -> evalExpr expr csv row == ""
    IsNotNull expr -> evalExpr expr csv row /= ""
    InList expr list -> evalExpr expr csv row `elem` map (\e -> evalExpr e csv row) list
    Match colIdx1 colIdx2 ->
        let idx1 = resolveColIndex csv [] colIdx1
            idx2 = resolveColIndex csv [] colIdx2
        in (not (idx1 >= length row || idx2 >= length row) && (row !! idx1 == row !! idx2))
    IsEmpty colIdx ->
        let idx = resolveColIndex csv [] colIdx
        in ((idx >= length row) || (row !! idx == ""))
    RowFilter rowNum -> rowNum >= 0 && rowNum < length csv && row == csv !! rowNum
    NotEmpty colIdx ->
        let idx = resolveColIndex csv [] colIdx
        in ((idx < length row) && (row !! idx /= ""))
    ExistsCol colIdx ->
        let idx = resolveColIndex csv [] colIdx
        in ((idx < length row) && (row !! idx /= ""))


-- | Evaluate an expression against a row
evalExpr :: Expr -> CSV -> Row -> String
evalExpr expr csv row = case expr of
    ColRef colIdx ->
        let idx = resolveColIndex csv [] colIdx
        in if idx >= 0 && idx < length row then row !! idx else ""
    StringLit str -> str
    IntLit n -> show n
    BinaryOp op expr1 expr2 ->
        let val1 = evalExpr expr1 csv row
            val2 = evalExpr expr2 csv row
        in case op of
            Add -> 
                -- Try to parse as numbers first
                case (reads val1 :: [(Double, String)], reads val2 :: [(Double, String)]) of
                    ([(n1, "")], [(n2, "")]) -> show (n1 + n2) -- Numerical addition
                    _ -> val1 ++ val2  -- String concatenation fallback
            Subtract -> 
                case (reads val1 :: [(Double, String)], reads val2 :: [(Double, String)]) of
                    ([(n1, "")], [(n2, "")]) -> show (n1 - n2)
                    _ -> error "Subtraction requires numeric values"
            Multiply -> 
                case (reads val1 :: [(Double, String)], reads val2 :: [(Double, String)]) of
                    ([(n1, "")], [(n2, "")]) -> show (n1 * n2)
                    _ -> error "Multiplication requires numeric values"
            Divide -> 
                case (reads val1 :: [(Double, String)], reads val2 :: [(Double, String)]) of
                    ([(n1, "")], [(n2, "")]) -> 
                        if n2 == 0 
                            then error "Division by zero"
                            else show (n1 / n2)
                    _ -> error "Division requires numeric values"

-- | Resolve a column index to an actual integer index
resolveColIndex :: CSV -> [String] -> ColIndex -> Int
resolveColIndex csv headers colIdx = case colIdx of
    IndexBased n -> n - 1  -- Convert 1-based to 0-based indexing
    NameBased name ->
        if null headers
            then if null csv then -1 else 0  -- Default to first column if no headers
            else fromMaybe (-1) $ findIndex (== name) headers
    NestedField _ _ -> error "Nested fields not supported" -- This would require more complex logic
    ArrayAccess _ _ -> error "Array access not supported"  -- This would require more complex logic

-- | Find the index of an element in a list
findIndex :: (a -> Bool) -> [a] -> Maybe Int
findIndex f = findIndexHelper 0
  where
    findIndexHelper _ [] = Nothing
    findIndexHelper i (x:xss)
        | f x = Just i
        | otherwise = findIndexHelper (i+1) xss

-- | Resolve column list to actual column indices
resolveColumnList :: ColumnList -> CSV -> Either String [ColumnResolution]
resolveColumnList cols csv = case cols of
    AllColumns ->
        if null csv
            then Right []
            else Right [DirectColumn i | i <- [0..(length (head csv) - 1)]]
    SpecificColumns colExprs ->
        mapM (resolveColExpr csv) colExprs

resolveColExpr :: CSV -> ColumnExpr -> Either String ColumnResolution
resolveColExpr csv (SimpleColumn colIdx) = 
    Right $ DirectColumn $ resolveColIndex csv [] colIdx
resolveColExpr csv (ColumnAlias colIdx _) = 
    Right $ DirectColumn $ resolveColIndex csv [] colIdx
resolveColExpr _ (StringColumn str) = 
    Right $ ConstantColumn str
resolveColExpr _ (ExprColumn expr) = 
    Right $ ExpressionColumn expr
resolveColExpr _ (ExprColumnAlias expr _) = 
    Right $ ExpressionColumn expr

-- | Project columns from source data to result data
projectColumns :: [ColumnResolution] -> CSV -> CSV -> CSV
projectColumns columns sourceData resultData =
    map (projectRow sourceData) resultData
  where
    projectRow :: CSV -> Row -> Row
    projectRow source row = 
        map (resolveColumnValue source row) columns
    
    resolveColumnValue :: CSV -> Row -> ColumnResolution -> String
    resolveColumnValue _ row (DirectColumn idx)
        | idx >= 0 && idx < length row = row !! idx
        | otherwise = ""
    resolveColumnValue source row (ExpressionColumn expr) = 
        evalExpr expr source row
    resolveColumnValue _ _ (ConstantColumn str) = 
        str

-- | Helper for concatMap with Either monad
concatMapM :: (a -> Either String [b]) -> [a] -> Either String [b]
concatMapM f = foldl (\acc x -> do
                          accResult <- acc
                          xResult <- f x
                          return (accResult ++ xResult)) (Right [])

-- | Load and run a query file
runQueryFile :: FilePath -> IO (Either String CSV)
runQueryFile filePath = do
    fileExists <- doesFileExist filePath
    if fileExists
        then do
            queryStr <- readFile filePath
            interpretQuery queryStr
        else return $ Left $ "Query file not found: " ++ filePath

-- | Run a query on specific CSV files
-- | Run a query on specific CSV files
runQueryOnFiles :: String -> [FilePath] -> IO (Either String CSV)
runQueryOnFiles queryStr _ = do
    let tokens = lexer queryStr
    case tokens of
        [] -> return $ Left "Empty query"
        _ -> do
            let queries = parseQueries tokens
            runQueries queries Map.empty  -- Start with empty tables

data ColumnResolution = 
    DirectColumn Int           -- Index of a direct column reference
  | ExpressionColumn Expr      -- Expression to evaluate for each row
  | ConstantColumn String      -- Constant string value
  deriving (Show)