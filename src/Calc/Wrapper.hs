{-# LANGUAGE OverloadedStrings          #-}

module Calc.Wrapper
  ( parse
  , formatError
  , Error(..)
  , ErrClass(..)
  )
  where

----------------------------------------------------------------------------
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.List            (isPrefixOf)
----
import           Calc.Base            (runAlex)
import           Calc.Data            (Exp)
import           Calc.Parser          (happyParser)
----------------------------------------------------------------------------

data ErrClass
    = Syntactical (Maybe String)
    | Lexical
    | Message String
    deriving (Show, Eq)

data Error = Error
    { errLine  :: Int
    , errPos   :: Int
    , errClass :: ErrClass
    } deriving (Show, Eq)

formatError :: BL.ByteString -> BL.ByteString -> Error -> BL.ByteString
formatError filename content (Error line pos errclass) =
    BL.concat $ [filename, ":", int line, ":", int pos, " ", BL8.pack $ strErr errclass, "\n"] ++
                strContent (take 1 $ drop (line - 1) $ BL8.lines content)
    where
        strContent [lineContent] =
            [lineContent, "\n",
             BL8.replicate (fromIntegral (pos - 1)) ' ', "^", "\n"
            ]
        strContent _ = []
        strErr (Syntactical Nothing)    = "syntax error"
        strErr (Syntactical (Just str)) = "syntax error: " ++ str
        strErr (Lexical)                = "lexical error"
        strErr (Message str)            = "error: " ++ str
        int = BL8.pack . show

parse :: BL.ByteString -> Either Error Exp
parse s =
    -- Alex's error type is a String, that we have to parse here,
    -- otherwise we cannot get type-safe information out of 'parse'.
    let showErrPrefix = "show-error: " :: String
        lexicalErrorPrefix = "lexical error at line " :: String
     in case runAlex s $ happyParser of
            Right x -> Right x
            Left str | showErrPrefix `isPrefixOf` str ->
                          let (line, column, m) =
                                  (read (drop (length showErrPrefix) str) :: (Int, Int, Maybe String))
                           in Left (Error line column (Syntactical m))
                     | lexicalErrorPrefix `isPrefixOf` str ->
                          let info = drop (length lexicalErrorPrefix) str
                              lineStr = takeWhile (/= ',') info
                              columnStr = drop (9 + length lineStr) info
                           in Left (Error (read lineStr) (read columnStr) Lexical)
                     | otherwise  -> Left (Error 0 0 (Message str))
