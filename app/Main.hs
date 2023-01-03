module Main (main) where

import Data.Vector (Vector)
import Data.Csv
    ( (.:),
      decodeByNameWithP,
      defaultDecodeOptions,
      Parser,
      NamedRecord )
import qualified Data.ByteString.Lazy as BL
import Data.Either.Combinators ( mapRight )

data DataPoint = DataPoint { date :: String, closePrice :: Double } deriving (Show, Eq, Ord)

dataPointParser :: NamedRecord -> Parser DataPoint
dataPointParser r = DataPoint <$> r .: "date" <*> r .: "close"

type ParseResult = Either String (Vector DataPoint)
parseData :: BL.ByteString -> ParseResult
parseData input = mapRight snd $ decodeByNameWithP dataPointParser defaultDecodeOptions input

getData :: IO ParseResult
getData = do
    contents <- BL.readFile "/home/murban/git/averaging/data/spx.csv" 
    let records = parseData contents
    return records

main :: IO ()
main = do
    parseResult <- getData
    case parseResult of
      Left err ->
          print err
      Right x ->
          print x
