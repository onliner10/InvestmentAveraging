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
import Core (DataPoint (DataPoint), ParseResult, ClosePrice (ClosePrice))
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import Money (denseFromDecimal, defaultDecimalConf)

parseDay :: NamedRecord -> BS.ByteString -> Parser Day
parseDay r field = do
    stringVal <- r .: field
    parseTimeM True defaultTimeLocale "%d-%b-%y" stringVal

closePriceParser :: NamedRecord -> Parser ClosePrice
closePriceParser r = do
    closePriceM <- denseFromDecimal defaultDecimalConf <$> r .: "close"
    closePrice <- maybe (fail "Couldn't convert to money") return closePriceM

    return $ ClosePrice closePrice

dataPointParser :: NamedRecord -> Parser (DataPoint ClosePrice)
dataPointParser r = DataPoint <$> r `parseDay` "date" <*> closePriceParser r

parseData :: BL.ByteString -> ParseResult
parseData input = mapRight snd $ decodeByNameWithP dataPointParser defaultDecodeOptions input

getData :: IO ParseResult
getData = do
    contents <- BL.readFile "/home/murban/git/averaging/data/spx.csv" 
    let records = parseData contents
    return records
