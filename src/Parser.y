{
module Parser where

import Lexer
import Data.Maybe (fromMaybe)
}

%name parseQuery
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
  '#'         { PT _ TokenHash }
  identifier  { PT _ (TokenIdentifier $$) }
  string      { PT _ (TokenString $$) }
  int         { PT _ (TokenInt $$) }

-- Define precedence and associativity
%left OR
%left AND
%nonassoc '=' '!=' '<' '>' '<=' '>='
%left '+' '-'
%left '*' '/'

%%

-- Main query structure
Query : QueryExpr ';'                      { $1 }
      | QueryExpr                          { $1 }

QueryExpr : SelectQuery                    { $1 }
          | CartesianProduct               { $1 }
          | PermuteDrop                    { $1 }
          | ExistsCheck                    { $1 }
          | CopyAndConstants               { $1 }
          | LeftMerge                      { $1 }
          | Project                        { $1 }
          | RenameOperation                { $1 }  -- New
          | CreateOperation                { $1 }  -- New

-- SELECT query format
SelectQuery : SELECT ColumnList FROM TableExpr OptionalWhere
                                             { Select $2 $4 $5 }
            | SELECT DISTINCT ColumnList FROM TableExpr OptionalWhere
                                             { SelectDistinct $3 $5 $6 }  -- New

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
                                             { ProjectGroupBy $2 $4 $6 }  -- New

-- RENAME operation (new)
RenameOperation : RENAME ColIndex TO identifier FROM TableExpr
                                             { RenameColumn $2 $4 $6 }

-- CREATE operation (new)
CreateOperation : CREATE identifier AS QueryExpr
                                             { CreateTable $2 $4 }

-- Column list for SELECT statements
ColumnList : '*'                             { AllColumns }
           | ColumnExprList                  { SpecificColumns $1 }

ColumnExprList : ColumnExpr                  { [$1] }
               | ColumnExprList ',' ColumnExpr
                                             { $1 ++ [$3] }

ColumnExpr : ColIndex                        { SimpleColumn $1 }
           | ColIndex AS identifier          { ColumnAlias $1 $3 }

-- Table expressions
TableExpr : TableTerm                        { $1 }
          | TableExpr JOIN TableTerm ON Condition
                                             { Join $1 $3 $5 }

TableTerm : identifier                       { Table $1 }
          | '(' QueryExpr ')'                { SubQuery $2 }

TableList : TableExpr                        { [$1] }
          | TableList ',' TableExpr          { $1 ++ [$3] }

-- Renamed WhereClause to OptionalWhere for clarity
OptionalWhere : WHERE Condition             { Just $2 }
              |                             { Nothing }
              | WHERE ROW int               { Just (RowFilter $3) }  -- New

-- Conditions for WHERE clause
Condition : Condition AND Condition          { And $1 $3 }
          | Condition OR Condition           { Or $1 $3 }
          | '(' Condition ')'                { $2 }
          | Expr '=' Expr                    { Equals $1 $3 }
          | Expr '!=' Expr                   { NotEquals $1 $3 }
          | Expr '<' Expr                    { LessThan $1 $3 }
          | Expr '>' Expr                    { GreaterThan $1 $3 }
          | Expr '<=' Expr                   { LessEquals $1 $3 }
          | Expr '>=' Expr                   { GreaterEquals $1 $3 }
          | Expr IS NULL                     { IsNull $1 }
          | Expr IS NOT NULL                 { IsNotNull $1 }
          | Expr IN '(' ExprList ')'         { InList $1 $4 }
          | MATCH ColIndex ColIndex          { Match $2 $3 }
          | EMPTY ColIndex                   { IsEmpty $2 }

-- Modified Expressions to remove the ColReference intermediary
-- and explicitly tag column references
Expr : COLREF ColIndex                      { ColRef $2 }
     | Literal                               { $1 }
     | Expr '+' Expr                         { BinaryOp Add $1 $3 }
     | Expr '-' Expr                         { BinaryOp Subtract $1 $3 }
     | Expr '*' Expr                         { BinaryOp Multiply $1 $3 }
     | Expr '/' Expr                         { BinaryOp Divide $1 $3 }
     | '(' Expr ')'                          { $2 }

-- Column indices with more explicit syntax
ColIndex : '#' int                           { IndexBased $2 }
         | identifier                        { NameBased $1 }
         | identifier '.' identifier         { NestedField $1 $3 }  -- New
         | identifier '[' int ']'            { ArrayAccess $1 $3 }  -- New

ColIndices : ColIndex                        { [$1] }
           | ColIndices ',' ColIndex         { $1 ++ [$3] }

-- Literals
Literal : string                             { StringLit $1 }
        | int                                { IntLit $1 }

-- List of expressions
ExprList : Expr                              { [$1] }
         | ExprList ',' Expr                 { $1 ++ [$3] }

{
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
  | SelectDistinct ColumnList TableExpr (Maybe Condition)  -- New
  | CartesianProduct [TableExpr]
  | Permute [ColIndex] TableExpr
  | Drop [ColIndex] TableExpr
  | PermuteWhere [ColIndex] TableExpr Condition
  | Exists ColIndex TableExpr
  | CopyWithConstant ColIndex String TableExpr
  | LeftMerge TableExpr ColIndex TableExpr
  | Project [ColIndex] TableExpr
  | ProjectGroupBy [ColIndex] TableExpr [ColIndex]  -- New
  | RenameColumn ColIndex String TableExpr  -- New
  | CreateTable String QueryExpr  -- New
  deriving (Show)

-- Column selection
data ColumnList = 
    AllColumns
  | SpecificColumns [ColumnExpr]
  deriving (Show)

data ColumnExpr = 
    SimpleColumn ColIndex
  | ColumnAlias ColIndex String
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
  | RowFilter Int  -- New
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
  deriving (Show)

-- Column references
data ColIndex = 
    IndexBased Int
  | NameBased String
  | NestedField String String  -- New
  | ArrayAccess String Int     -- New
  deriving (Show)
}