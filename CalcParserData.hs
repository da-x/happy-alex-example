module CalcParserData
    (
      Exp(..)
    , Term(..)
    , Exp1(..)
    , Factor(..)
    , AlexPosn
    ) where

import CalcLexer (AlexPosn)

data Exp  = Let AlexPosn String Exp Exp | Exp1 Exp1
    deriving (Show, Eq)

data Exp1 = Plus Exp1 Term | Minus Exp1 Term | Term Term
    deriving (Show, Eq)

data Term = Times Term Factor | Div Term Factor | Factor Factor
    deriving (Show, Eq)

data Factor = Int Int | Var String | Brack Exp
    deriving (Show, Eq)

