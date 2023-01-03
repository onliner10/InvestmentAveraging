module Core (DataPoint (DataPoint), ParseResult) where
import Data.Vector (Vector)
import Data.Time (Day)

data DataPoint = DataPoint { date :: Day, closePrice :: Double } deriving (Show, Eq, Ord)
type ParseResult = Either String (Vector DataPoint)
