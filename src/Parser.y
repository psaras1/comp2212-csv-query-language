{
module Parser where

import Lexer
import Data.Maybe (fromMaybe)
}

%name parseQueryList QueryList
%tokentype { PosnToken }
%error { parseError }

%token
  SELECT      { PT _ TokenSelect }
  FROM        { PT _ TokenFrom }
  WHERE       { PT _ TokenWhere }
  JOIN        { PT _ TokenJoin }
  ON          { PT _ TokenOn }
  AND         { PT _ TokenAnd }
  OR          { PT _ TokenOr }
  IN          { PT _ TokenIn }
  AS          { PT _ TokenAs }
  IS          { PT _ TokenIs }
  NULL        { PT _ TokenNull }
  NOT         { PT _ TokenNot }
  DISTINCT    { PT _ TokenDistinct }
  CARTESIAN   { PT _ TokenCartesian }
  PRODUCT     { PT _ TokenProduct }
  LEFT        { PT _ TokenLeft }
  MERGE       { PT _ TokenMerge }
  EXISTS      { PT _ TokenExists }
  PERMUTE     { PT _ TokenPermute }
  DROP        { PT _ TokenDrop }
  COPY        { PT _ TokenCopy }
  CONSTANT    { PT _ TokenConstant }
  RENAME      { PT _ TokenRename }
  CREATE      { PT _ TokenCreate }
  PROJECT     { PT _ TokenProject }
  BY          { PT _ TokenBy }
  ROW         { PT _ TokenRow }
  COL         { PT _ TokenCol }
  TO          { PT _ TokenTo }
  WITH        { PT _ TokenWith }
  EMPTY       { PT _ TokenEmpty }
  MATCH       { PT _ TokenMatch }
  COLREF      { PT _ TokenColRef }
  UNION       { PT _ TokenUnion }
  CONCAT      { PT _ TokenConcat }
  '='         { PT _ TokenEq }
  ','         { PT _ TokenComma }
  ';'         { PT _ TokenSemicolon }
  '('         { PT _ TokenLParen }
  ')'         { PT _ TokenRParen }
  '['         { PT _ TokenLBracket }
  ']'         { PT _ TokenRBracket }
  '*'         { PT _ TokenStar }
  '.'         { PT _ TokenDot }
  '+'         { PT _ TokenPlus }
  '-'         { PT _ TokenMinus }
  '/'         { PT _ TokenSlash }
  '>'         { PT _ TokenGreater }
  '<'         { PT _ TokenLess }
  '>='        { PT _ TokenGreaterEq }
  '<='        { PT _ TokenLessEq }
  '!='        { PT _ TokenNotEq }
  '||'        { PT _ TokenConcat }
  '#'         { PT _ TokenHash }
  identifier  { PT _ (TokenIdentifier $$) }
  string      { PT _ (TokenString $$) }
  int         { PT _ (TokenInt $$) }

-- Define precedence and associativity
%left UNION
%left OR
%left AND
%nonassoc '=' '!=' '<' '>' '<=' '>='
%left '+' '-' '||'
%left '*' '/'

%%

-- Main query structure
QueryList : Query                    { [$1] }
          | QueryList Query          { $2 : $1 }

Query : QueryExpr ';'                { $1 }
      | QueryExpr                    { $1 }
      
-- Split out union operations for clarity
QueryExpr : SimpleQuery                    { $1 }
          | QueryExpr UNION SimpleQuery    { Union $1 $3 }

SimpleQuery : SelectQuery                  { $1 }
            | CartesianProduct             { $1 }
            | PermuteDrop                  { $1 }
            | ExistsCheck                  { $1 }
            | CopyAndConstants             { $1 }
            | LeftMerge                    { $1 }
            | Project                      { $1 }
            | RenameOperation              { $1 }
            | CreateOperation              { $1 }
            | ParenQuery                   { $1 }
            | ConcatOperation               { $1 }

ParenQuery : '(' QueryExpr ')'             { $2 }

-- CONCAT operation
ConcatOperation : CONCAT '(' Expr ',' Expr ')' FROM TableExpr OptionalWhere
                                            { ConcatOp $3 $5 $8 $9 }


-- SELECT query format
SelectQuery : SELECT ColumnList FROM TableExpr OptionalWhere
                                             { Select $2 $4 $5 }
            | SELECT DISTINCT ColumnList FROM TableExpr OptionalWhere
                                             { SelectDistinct $3 $5 $6 }  

-- Cartesian product operation
CartesianProduct : CARTESIAN PRODUCT TableList
                                             { CartesianProduct $3 }

-- Permute and drop operations - restructured to avoid conflicts
PermuteDrop : PermuteExpr                    { $1 }
            | DropExpr                       { $1 }

PermuteExpr : PERMUTE ColIndices FROM TableExpr OptionalWhere
                                             { case $5 of
                                                Nothing -> Permute $2 $4
                                                Just cond -> PermuteWhere $2 $4 cond }

DropExpr : DROP ColIndices FROM TableExpr    { Drop $2 $4 }

-- Exists check
ExistsCheck : EXISTS COL ColIndex FROM TableExpr
                                             { Exists $3 $5 }

-- Copy and constants operation
CopyAndConstants : COPY ColIndex WITH CONSTANT string FROM TableExpr
                                             { CopyWithConstant $2 $5 $7 }

-- Left merge operation
LeftMerge : LEFT MERGE TableExpr ON ColIndex WITH TableExpr
                                             { LeftMerge $3 $5 $7 }

-- Project operation
Project : PROJECT ColIndices FROM TableExpr
                                           { Project $2 $4 }
        | PROJECT ColIndices FROM TableExpr BY ColIndices
                                           { ProjectGroupBy $2 $4 $6 }
        | PROJECT ColIndices FROM TableExpr WHERE Condition
                                           { ProjectWhere $2 $4 $6 }

-- RENAME operation
RenameOperation : RENAME ColIndex TO identifier FROM TableExpr
                                             { RenameColumn $2 $4 $6 }

-- CREATE operation
CreateOperation : CREATE identifier AS SimpleQuery
                                             { CreateTable $2 $4 }

-- Column list for SELECT statements
ColumnList : '*'                             { AllColumns }
           | ColumnExprList                  { SpecificColumns $1 }
           | ColumnExprList ',' string       { SpecificColumns ($1 ++ [StringColumn $3]) }

ColumnExprList : ColumnExpr                  { [$1] }
               | ColumnExprList ',' ColumnExpr
                                             { $1 ++ [$3] }

ColumnExpr : SimpleColExpr          { $1 }
           | ComplexExpr            { $1 }

SimpleColExpr : ColIndex                { SimpleColumn $1 }
              | ColIndex AS identifier  { ColumnAlias $1 $3 }

ComplexExpr : ComplexExprNoAlias              { ExprColumn $1 }
            | ComplexExprNoAlias AS identifier { ExprColumnAlias $1 $3 }

ComplexExprNoAlias : ArithExpr               { $1 }
                   | ParenExpr               { $1 }
-- Table expressions
TableExpr : TableTerm                        { $1 }
          | TableExpr JOIN TableTerm ON Condition
                                             { Join $1 $3 $5 }

TableTerm : identifier                       { Table $1 }
          | SubQueryExpr                     { $1 }

SubQueryExpr : '(' QueryExpr ')'             { SubQuery $2 }

TableList : TableExpr                        { [$1] }
          | TableList ',' TableExpr          { $1 ++ [$3] }

-- Renamed WhereClause to OptionalWhere for clarity
OptionalWhere : WHERE Condition             { Just $2 }
              |                             { Nothing }
              | WHERE ROW int               { Just (RowFilter $3) }

-- Conditions for WHERE clause
Condition : Condition AND Condition          { And $1 $3 }
          | Condition OR Condition           { Or $1 $3 }
          | ParenCondition                   { $1 }
          | SimpleCondition                  { $1 }

ParenCondition : '(' Condition ')'           { $2 }

SimpleCondition : Expr '=' Expr              { Equals $1 $3 }
                | Expr '!=' Expr             { NotEquals $1 $3 }
                | Expr '<' Expr              { LessThan $1 $3 }
                | Expr '>' Expr              { GreaterThan $1 $3 }
                | Expr '<=' Expr             { LessEquals $1 $3 }
                | Expr '>=' Expr             { GreaterEquals $1 $3 }
                | Expr IS NULL               { IsNull $1 }
                | Expr IS NOT NULL           { IsNotNull $1 }
                | Expr IN '(' ExprList ')'   { InList $1 $4 }
                | MATCH ColIndex ColIndex    { Match $2 $3 }
                | EMPTY ColIndex             { IsEmpty $2 }
                | NOT EMPTY ColIndex         { NotEmpty $3 }
                | EXISTS COL ColIndex        { ExistsCol $3 }

-- Modified Expressions rule to handle column references directly
Expr : COLREF ColIndex              { ColRef $2 }
     | ColIndex                     { ColRef $1 }
     | Literal                      { $1 }
     | ArithExpr                    { $1 }
     | ParenExpr                    { $1 }
     
ArithExpr : Expr '+' Expr                    { BinaryOp Add $1 $3 }
          | Expr '-' Expr                    { BinaryOp Subtract $1 $3 }
          | Expr '*' Expr                    { BinaryOp Multiply $1 $3 }
          | Expr '/' Expr                    { BinaryOp Divide $1 $3 }
          | Expr '||' Expr                   { BinaryOp Concat $1 $3} -- new (infix form)
          | CONCAT '(' Expr ',' Expr ')'     { BinaryOp Concat $3 $5} -- new (function form)


ParenExpr : '(' Expr ')'                     { $2 }

-- Column indices with more explicit syntax
ColIndex : '#' int                           { IndexBased $2 }
         | identifier                        { NameBased $1 }
         | identifier '.' identifier         { NestedField $1 $3 }
         | identifier '[' int ']'            { ArrayAccess $1 $3 }

ColIndices : ColIndex                        { [$1] }
           | ColIndices ',' ColIndex         { $1 ++ [$3] }

-- Literals
Literal : string                             { StringLit $1 }
        | int                                { IntLit $1 }

-- List of expressions
ExprList : Expr                              { [$1] }
         | ExprList ',' Expr                 { $1 ++ [$3] }

{
-- Helper function to convert the query list to a more usable form
parseQueries :: [PosnToken] -> [QueryExpr]
parseQueries = reverse . parseQueryList

-- Error handling
parseError :: [PosnToken] -> a
parseError [] = error "Parse error: unexpected end of input"
parseError (PT pos t:_) = error $ "Parse error at line " ++ show line ++ ", column " ++ show column ++ ": unexpected " ++ show t
  where
    (AlexPn _ line column) = pos

-- Data Structures for AST

-- Main query operations
data QueryExpr = 
    Select ColumnList TableExpr (Maybe Condition)
  | SelectDistinct ColumnList TableExpr (Maybe Condition)  
  | CartesianProduct [TableExpr]
  | Permute [ColIndex] TableExpr
  | Drop [ColIndex] TableExpr
  | PermuteWhere [ColIndex] TableExpr Condition
  | Exists ColIndex TableExpr
  | CopyWithConstant ColIndex String TableExpr
  | LeftMerge TableExpr ColIndex TableExpr
  | Project [ColIndex] TableExpr
  | ProjectWhere [ColIndex] TableExpr Condition
  | ProjectGroupBy [ColIndex] TableExpr [ColIndex]  
  | RenameColumn ColIndex String TableExpr 
  | CreateTable String QueryExpr  
  | Union QueryExpr QueryExpr 
  | ConcatOp Expr Expr TableExpr (Maybe Condition)
  deriving (Show)

-- Column selection
data ColumnList = 
    AllColumns
  | SpecificColumns [ColumnExpr]
  deriving (Show)

data ColumnExpr = 
    SimpleColumn ColIndex
  | ColumnAlias ColIndex String
  | StringColumn String
  | ExprColumn Expr
  | ExprColumnAlias Expr String
  deriving (Show)

-- Table expressions
data TableExpr = 
    Table String
  | SubQuery QueryExpr
  | Join TableExpr TableExpr Condition
  deriving (Show)

-- Conditions for filtering
data Condition = 
    And Condition Condition
  | Or Condition Condition
  | Equals Expr Expr
  | NotEquals Expr Expr
  | LessThan Expr Expr
  | GreaterThan Expr Expr
  | LessEquals Expr Expr
  | GreaterEquals Expr Expr
  | IsNull Expr
  | IsNotNull Expr
  | InList Expr [Expr]
  | Match ColIndex ColIndex
  | IsEmpty ColIndex
  | NotEmpty ColIndex
  | RowFilter Int
  | ExistsCol ColIndex
  deriving (Show)

-- Expressions
data Expr = 
    ColRef ColIndex
  | StringLit String
  | IntLit Int
  | BinaryOp BinOp Expr Expr
  deriving (Show)

data BinOp = 
    Add
  | Subtract
  | Multiply
  | Divide
  | Concat   --new binary operation
  deriving (Show)

-- Column references
data ColIndex = 
    IndexBased Int
  | NameBased String
  | NestedField String String
  | ArrayAccess String Int
  deriving (Show)
}