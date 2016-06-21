module CalcParserBase
  ( thenP
  , returnP
  , happyError
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

import CalcLexer

-- For readablity - this are the interface Happy expects:

type Parser a = Alex a

thenP :: Parser a -> (a -> Parser b) -> Parser b
thenP = (>>=)

returnP :: a -> Parser a
returnP = return

alexShowError (line, column, e) = alexError $ "show-error: " ++ (show (line, column, e))

alexGetPosition :: Alex (AlexPosn)
alexGetPosition = Alex $ \s@AlexState{alex_pos=pos} -> Right (s, pos)

happyError :: Parser a
happyError = do
  (AlexPn _ line col) <- alexGetPosition
  alexShowError (line, col, "syntax error" :: String)

-- Link the lexer and the parser:

lexer :: (Token -> Parser a) -> Parser a
lexer f = alexMonadScan >>= f
