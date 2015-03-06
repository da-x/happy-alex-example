module CalcParserUser
  ( parse
  )
  where

import qualified Data.ByteString.Lazy    as BL
import           CalcParserBase          (Exp, runAlex)
import           CalcParser              (happyParser)

parse :: BL.ByteString -> Either String Exp
parse s = runAlex s $ happyParser
