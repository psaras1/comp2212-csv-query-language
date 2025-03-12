module CSV (
    -- Data types
    CSV,
    Row,
    Cell,
    -- Functions
    parseCSV,
    writeCSV,
    readCSVFile,
    writeCSVFile,
    -- Utility functions
    trim,
    sortCSV
) where

import Data.List (sort)
import System.IO (readFile, writeFile)
import Control.Exception (catch, IOException)
import qualified Data.Char as Char

-- Type definitions
type Cell = String
type Row = [Cell]
type CSV = [Row]

-- | Parse a string into a CSV (list of rows)
parseCSV :: String -> CSV
parseCSV str = map parseLine (filter (not . null) (lines str))
  where
    parseLine :: String -> Row
    parseLine line = map trim (splitOnComma line)
    
    splitOnComma :: String -> [String]
    splitOnComma [] = [""]
    splitOnComma str = 
      case break (==',') str of
        (l, ',':r) -> l : splitOnComma r
        (l, "")    -> [l]

-- | Convert a CSV to a string
writeCSV :: CSV -> String
writeCSV = unlines . map joinWithComma
  where
    joinWithComma = concat . intersperse ","
    
    intersperse _ [] = []
    intersperse _ [x] = [x]
    intersperse sep (x:xs) = x : sep : (intersperse sep xs)

-- | Read a CSV file and return its contents
readCSVFile :: FilePath -> IO (Either String CSV)
readCSVFile filePath = do
    result <- (Right <$> readFile filePath) `catch` handleIOError
    return $ case result of
        Right content -> Right (parseCSV content)
        Left err      -> Left err
  where
    handleIOError :: IOException -> IO (Either String a)
    handleIOError e = return $ Left $ "Error reading file: " ++ show e

-- | Write CSV data to a file
writeCSVFile :: FilePath -> CSV -> IO (Either String ())
writeCSVFile filePath csv = do
    (Right <$> writeFile filePath (writeCSV csv)) `catch` handleIOError
  where
    handleIOError :: IOException -> IO (Either String ())
    handleIOError e = return $ Left $ "Error writing file: " ++ show e

-- | Sort a CSV lexicographically by all columns
sortCSV :: CSV -> CSV
sortCSV = sort

-- | Trim whitespace from the beginning and end of a string
trim :: String -> String
trim = reverse . dropWhile Char.isSpace . reverse . dropWhile Char.isSpace