{-# LANGUAGE OverloadedStrings #-}

import           CalcParserUser (parse)

main :: IO ()
main = do
  -- Should work
  print $ parse "1 + 2 + 3"
  -- Should fail in lexer
  print $ parse "1 + 2 + Z 3"
  -- Should fail in parser
  print $ parse "1 + 2 + let 3"
