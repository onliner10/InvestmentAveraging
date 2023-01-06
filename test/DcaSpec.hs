{-# OPTIONS_GHC -Wno-orphans #-}
module DcaSpec (main) where

import           Core               (ClosePrice (ClosePrice, closePrice),
                                     DataPoint (DataPoint, day, value),
                                     DataSet (DataSet, values),
                                     InvestmentSetup (InvestmentSetup, prices, wallet),
                                     Wallet (Wallet, amount))
import           Data.SortedList    (fromSortedList, toSortedList)
import           Data.Time          (addDays, TimeLocale (wDays), CalendarDiffDays (CalendarDiffDays))
import           Dca                (DcaInterval (DcaInterval),
                                     dolarCostAveraging)
import           Test.QuickCheck    (Arbitrary (arbitrary), choose, quickCheck, verboseCheck)
import           TimeArbitraries    ()
import qualified Data.Map as Map
import Data.AdditiveGroup
import qualified Money
import Debug.Trace

absMoney :: Money.Dense a -> Money.Dense a
absMoney x | x < zeroV = negateV x
absMoney x = x

instance Arbitrary ClosePrice where
  arbitrary = ClosePrice . absMoney <$> arbitrary

instance Arbitrary Wallet where
  arbitrary = Wallet . absMoney <$> arbitrary

instance Arbitrary DcaInterval where
    arbitrary = do
        days <- choose (1, 30)
        months <- choose (1,12)
        return $ DcaInterval $ CalendarDiffDays months days

instance (Ord a, Arbitrary a) =>Arbitrary (DataSet a) where
  arbitrary = do
    firstDay <- arbitrary
    daysIntoFuture <- choose (1, 1)

    let dayList = flip addDays firstDay <$> [0 .. daysIntoFuture]
    dataPoints <- mapM ((<$> arbitrary) . DataPoint) dayList

    let sortedDataPoints = toSortedList dataPoints
    return $ DataSet sortedDataPoints

instance Arbitrary InvestmentSetup where
  arbitrary = InvestmentSetup <$> arbitrary <*> arbitrary

prop_contributorsSumsUpToWallet :: InvestmentSetup -> DcaInterval -> Bool
prop_contributorsSumsUpToWallet setup interval =
    let
        expectedAmount = trace "Expected amount: " $ traceShowId $ amount $ wallet setup
        priceList = fromSortedList $ values $ prices setup
        pricesDaily = Map.fromList $ (\dp -> (day dp, closePrice $ value dp)) <$> priceList
        result = trace "Result" $ traceShowId $ fromSortedList $ dolarCostAveraging setup interval
        spentAmount = trace "Spent amount" $ traceShowId $ foldl (^+^) zeroV <$> mapM ((`Map.lookup` pricesDaily) . day) result
     in spentAmount == Just expectedAmount

main :: IO ()
main = do
  verboseCheck prop_contributorsSumsUpToWallet
