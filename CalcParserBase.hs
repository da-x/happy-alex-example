module CalcParserBase
  ( thenP
  , returnP
  , happyError
  , Parser
  , Token(..)
  , tokenToPosN
  , TokenClass(..)
  , Exp(..)
  , Exp1(..)
  , Term(..)
  , Factor(..)
  , AlexPosn
  , AlexState(..)
  , lexer
  , runAlex
  )
  where

import CalcLexer

-- For readablity - these are the interface Happy expects:

type Parser a = Alex a

thenP :: Parser a -> (a -> Parser b) -> Parser b
thenP = (>>=)

returnP :: a -> Parser a
returnP = return

happyError :: Parser a
happyError = Alex $ \AlexState{alex_pos=pos} -> error (
  "Parse error in " ++ show (pos) ++ "\n")

-- Now we define the data types of the Syntax

data Exp  = Let AlexPosn String Exp Exp | Exp1 Exp1
  deriving (Show)

data Exp1 = Plus Exp1 Term | Minus Exp1 Term | Term Term
  deriving (Show)

data Term = Times Term Factor | Div Term Factor | Factor Factor
  deriving (Show)

data Factor = Int Int | Var String | Brack Exp
  deriving (Show)

-- Link the lexer and the parser:

lexer :: (Token -> Parser a) -> Parser a
lexer f = alexMonadScan >>= f
