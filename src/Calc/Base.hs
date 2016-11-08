module Calc.Base
  ( thenP
  , returnP
  , handleErrorExpList
  , Parser
  , Token(..)
  , tokenToPosN
  , TokenClass(..)
  , AlexPosn
  , AlexState(..)
  , lexer
  , runAlex
  )
  where

----------------------------------------------------------------------------
import Calc.Lexer
import Data.List (intersperse)
----------------------------------------------------------------------------

-- For readablity - this are the interface Happy expects:

type Parser a = Alex a

thenP :: Parser a -> (a -> Parser b) -> Parser b
thenP = (>>=)

returnP :: a -> Parser a
returnP = return

alexShowError :: (Show t, Show t1) => (t, t1, Maybe String) -> Alex a
alexShowError (line, column, e) = alexError $ "show-error: " ++ (show (line, column, e))

alexGetPosition :: Alex (AlexPosn)
alexGetPosition = Alex $ \s@AlexState{alex_pos=pos} -> Right (s, pos)

handleErrorExpList :: (Token, [String]) -> Parser a
handleErrorExpList (Token _ cls, opts) = do
  (AlexPn _ line col) <- alexGetPosition
  let nextTokens xs = ", possible next tokens types: " ++ (concat $ intersperse " " xs)
  alexShowError (line, col, Just $ "syntax error: got token "
                 ++ (show $ tokenToStr cls)
                 ++ nextTokens opts)

-- Link the lexer and the parser:

lexer :: (Token -> Parser a) -> Parser a
lexer f = alexMonadScan >>= f
