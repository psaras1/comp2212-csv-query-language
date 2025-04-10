{
module Lexer where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$alphaNum = [a-zA-Z0-9]

tokens :-
  $white+                       ;
  "--".*                        ;
  "/*"([^\*]|[\r\n]|(\*+([^\*\/]|[\r\n])))*\*+"/"  ; -- Multi-line comments
  
  -- SQL-inspired keywords
  SELECT                        { \p s -> PT p TokenSelect }
  FROM                          { \p s -> PT p TokenFrom }
  WHERE                         { \p s -> PT p TokenWhere }
  JOIN                          { \p s -> PT p TokenJoin }
  ON                            { \p s -> PT p TokenOn }
  AND                           { \p s -> PT p TokenAnd }
  OR                            { \p s -> PT p TokenOr }
  IN                            { \p s -> PT p TokenIn }
  AS                            { \p s -> PT p TokenAs }
  IS                            { \p s -> PT p TokenIs }
  NULL                          { \p s -> PT p TokenNull }
  NOT                           { \p s -> PT p TokenNot }
  DISTINCT                      { \p s -> PT p TokenDistinct }
  CARTESIAN                     { \p s -> PT p TokenCartesian }
  PRODUCT                       { \p s -> PT p TokenProduct }
  LEFT                          { \p s -> PT p TokenLeft }
  MERGE                         { \p s -> PT p TokenMerge }
  EXISTS                        { \p s -> PT p TokenExists }
  PERMUTE                       { \p s -> PT p TokenPermute }
  DROP                          { \p s -> PT p TokenDrop }
  COPY                          { \p s -> PT p TokenCopy }
  CONSTANT                      { \p s -> PT p TokenConstant }
  RENAME                        { \p s -> PT p TokenRename }
  CREATE                        { \p s -> PT p TokenCreate }
  PROJECT                       { \p s -> PT p TokenProject }
  COLREF                        { \p s -> PT p TokenColRef }
  UNION                         { \p s -> PT p TokenUnion }
  CONCAT                        { \p s -> PT p TokenConcat }
  

  
  -- CSV-specific functions and operations
  BY                            { \p s -> PT p TokenBy }
  ROW                           { \p s -> PT p TokenRow }
  COL                           { \p s -> PT p TokenCol }
  TO                            { \p s -> PT p TokenTo }
  WITH                          { \p s -> PT p TokenWith }
  EMPTY                         { \p s -> PT p TokenEmpty }
  MATCH                         { \p s -> PT p TokenMatch }
  
  -- Symbols
  "="                           { \p s -> PT p TokenEq }
  ","                           { \p s -> PT p TokenComma }
  ";"                           { \p s -> PT p TokenSemicolon }
  "("                           { \p s -> PT p TokenLParen }
  ")"                           { \p s -> PT p TokenRParen }
  "["                           { \p s -> PT p TokenLBracket }
  "]"                           { \p s -> PT p TokenRBracket }
  "*"                           { \p s -> PT p TokenStar }
  "."                           { \p s -> PT p TokenDot }
  "+"                           { \p s -> PT p TokenPlus }
  "-"                           { \p s -> PT p TokenMinus }
  "/"                           { \p s -> PT p TokenSlash }
  ">"                           { \p s -> PT p TokenGreater }
  "<"                           { \p s -> PT p TokenLess }
  ">="                          { \p s -> PT p TokenGreaterEq }
  "<="                          { \p s -> PT p TokenLessEq }
  "!="                          { \p s -> PT p TokenNotEq }
  "#"                           { \p s -> PT p TokenHash }
-- "||"                          { \p s -> PT p TokenConcat }
  
  -- Identifiers and literals
  $alpha [$alphaNum\_]*         { \p s -> PT p (TokenIdentifier s) }
  \" ([^\"]|(\\\"))* \"         { \p s -> PT p (TokenString (strip s)) }
  $digit+                       { \p s -> PT p (TokenInt (read s)) }

{
-- Strip quotes from string literals
strip :: String -> String
strip s = init (tail s)

data PosnToken = PT AlexPosn Token deriving (Eq, Show)

data Token =
  -- SQL-inspired keywords
  TokenSelect |
  TokenFrom |
  TokenWhere |
  TokenJoin |
  TokenOn |
  TokenAnd |
  TokenOr |
  TokenIn |
  TokenAs |
  TokenIs |
  TokenNull |
  TokenNot |
  TokenDistinct |
  TokenCartesian |
  TokenProduct |
  TokenLeft |
  TokenMerge |
  TokenExists |
  TokenPermute |
  TokenDrop |
  TokenCopy |
  TokenConstant |
  TokenRename |
  TokenCreate |
  TokenProject |
  TokenColRef |
  TokenUnion |
  TokenConcat | --new token for string concat
  
  -- CSV-specific functions and operations
  TokenBy |
  TokenRow |
  TokenCol |
  TokenTo |
  TokenWith |
  TokenEmpty |
  TokenMatch |
  
  -- Symbols
  TokenEq |
  TokenComma |
  TokenSemicolon |
  TokenLParen |
  TokenRParen |
  TokenLBracket |
  TokenRBracket |
  TokenStar |
  TokenDot |
  TokenPlus |
  TokenMinus |
  TokenSlash |
  TokenGreater |
  TokenLess |
  TokenGreaterEq |
  TokenLessEq |
  TokenNotEq |
  TokenHash |
  
  -- Identifiers and literals
  TokenIdentifier String |
  TokenString String |
  TokenInt Int
  deriving (Eq, Show)

tokenPosn :: PosnToken -> AlexPosn
tokenPosn (PT p _) = p

-- Utility function to help with parsing
lexer :: String -> [PosnToken]
lexer = alexScanTokens
}