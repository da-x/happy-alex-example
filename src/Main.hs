{-# LANGUAGE OverloadedStrings #-}

module Main where

----------------------------------------------------------------------------
import           Calc
----------------------------------------------------------------------------

main :: IO ()
main = do
    let test str expected = do
            putStrLn ""
            print $ str
            print $ expected
            let result = parse str
            if expected == result
                then putStrLn $ "OK."
                else do putStrLn $ "Error: " ++ show result
                        putStrLn $ "Expected: " ++ show expected

    -- Should work
    test "1 + 2 + 3" $ Right (Exp1 (Plus (Plus (Term (Factor (Int 1))) (Factor (Int 2))) (Factor (Int 3))))

    -- Should fail in lexer
    test "1 + 2 + % 3" $ Left (Error {errLine = 1, errPos = 9, errClass = Lexical})

    -- Should fail in parser
    test "1 + 2 + let 3" $ Left (Error {errLine = 1, errPos = 12, errClass = Syntactical (Just "syntax error: got token \"let\", possible next tokens types: int var '('")})

