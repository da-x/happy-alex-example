{-# LANGUAGE OverloadedStrings #-}

import           CalcParserUser (parse, Error(..), ErrClass(..))
import           CalcParserData

main :: IO ()
main = do
  -- Should work
  print $ parse "1 + 2 + 3" == Right (Exp1 (Plus (Plus (Term (Factor (Int 1))) (Factor (Int 2))) (Factor (Int 3))))

  -- Should fail in lexer
  print $ parse "1 + 2 + % 3" == Left (Error {errLine = 1, errPos = 9, errClass = Lexical})

  -- Should fail in parser
  print $ parse "1 + 2 + let 3" == Left (Error {errLine = 1, errPos = 12, errClass = Syntactical})
