module CalcParserUser
  ( parse
  , Error(..)
  , ErrClass(..)
  )
  where

import qualified Data.ByteString.Lazy    as BL
import           CalcParserData          (Exp)
import           CalcParserBase          (runAlex)
import           CalcParser              (happyParser)
import Data.List (isPrefixOf)

data ErrClass
    = Syntactical
    | Lexical
    | Message String
    deriving (Show, Eq)

data Error = Error
    { errLine  :: Int
    , errPos   :: Int
    , errClass :: ErrClass
    } deriving (Show, Eq)

parse :: BL.ByteString -> Either Error Exp
parse s =
    -- Alex's error type is a String, that we have to parse here,
    -- otherwise we cannot get type-safe information out of 'parse'.
    let showErrPrefix = "show-error: " :: String
        lexicalErrorPrefix = "lexical error at line " :: String
     in case runAlex s $ happyParser of
            Right x -> Right x
            Left str | showErrPrefix `isPrefixOf` str ->
                          let (line, column, e) =
                                  (read (drop (length showErrPrefix) str) :: (Int, Int, String))
                           in Left (Error line column Syntactical)
                     | lexicalErrorPrefix `isPrefixOf` str ->
                          let info = drop (length lexicalErrorPrefix) str
                              lineStr = takeWhile (/= ',') info
                              columnStr = drop (9 + length lineStr) info
                           in Left (Error (read lineStr) (read columnStr) Lexical)
                     | otherwise  -> Left (Error 0 0 (Message str))
