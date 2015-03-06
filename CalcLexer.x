{
{-# LANGUAGE OverloadedStrings #-}
module CalcLexer
  ( Alex(..)
  , AlexPosn(..)
  , AlexState(..)
  , Token(..)
  , TokenClass(..)
  , alexMonadScan
  , tokenToPosN
  , runAlex
  )
where

import System.Exit
import qualified Data.ByteString.Lazy.Char8 as B
}

%wrapper "monadUserState-bytestring"

$digit = 0-9                    -- digits
$alpha = [a-zA-Z]               -- alphabetic characters

tokens :-

  $white+                               ;
  "--".*                                ;
  let                                   { tok          TokenLet }
  in                                    { tok          TokenIn }
  $digit+                               { tok_read     TokenInt }
  [\+]                                  { tok          TokenPlus }
  [\-]                                  { tok          TokenMinus }
  [\*]                                  { tok          TokenTimes }
  [\/]                                  { tok          TokenDiv }
  [=]                                   { tok          TokenEq }
  [\(]                                  { tok          TokenOB }
  [\)]                                  { tok          TokenCB }
  $alpha [$alpha $digit \_ \']*         { tok_string   TokenVar }

{

-- Some action helpers:
tok' f (p, _, input) len = return $ Token p (f (B.take (fromIntegral len) input))
tok x = tok' (\s -> x)
tok_string x = tok' (\s -> x (B.unpack s))
tok_read x = tok' (\s -> x (read (B.unpack s)))

-- The token type:
data Token = Token AlexPosn TokenClass
  deriving (Show)

tokenToPosN :: Token -> AlexPosn
tokenToPosN (Token p _) = p

data TokenClass
 = TokenLet
 | TokenIn
 | TokenInt    Int
 | TokenVar    String
 | TokenPlus
 | TokenMinus
 | TokenTimes
 | TokenDiv
 | TokenEq
 | TokenOB
 | TokenCB
 | TokenEOF
 deriving (Eq, Show)

alexEOF :: Alex Token
alexEOF = do
  (p, _, _) <- alexGetInput
  return $ Token p TokenEOF

type AlexUserState = ()
alexInitUserState = ()
}
