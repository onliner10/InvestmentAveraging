module Adapter ( getData ) where

import Data.Csv
    ( (.:),
      decodeByNameWithP,
      defaultDecodeOptions,
      Parser,
      NamedRecord )
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Internal as BS
import Data.Either.Combinators ( mapRight )
import Core (DataPoint (DataPoint), ParseResult)
import Data.Time (Day, defaultTimeLocale, parseTimeM)

parseDay :: NamedRecord -> BS.ByteString -> Parser Day
parseDay r field = do
    stringVal <- r .: field
    parseTimeM True defaultTimeLocale "%d-%b-%y" stringVal

dataPointParser :: NamedRecord -> Parser DataPoint
dataPointParser r = DataPoint <$> r `parseDay` "date" <*> r .: "close"

parseData :: BL.ByteString -> ParseResult
parseData input = mapRight snd $ decodeByNameWithP dataPointParser defaultDecodeOptions input

getData :: IO ParseResult
getData = do
    contents <- BL.readFile "/home/murban/git/averaging/data/spx.csv" 
    let records = parseData contents
    return records
