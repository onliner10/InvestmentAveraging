{-# LANGUAGE DataKinds #-}
module Dca (dolarCostAveraging, DolarCostAveragingInterval) where
import           Core            (BoughtUnits (BoughtUnits), ClosePrice (closePrice),
                                  DataPoint (DataPoint, day, value),
                                  DataSet (DataSet, values),
                                  InvestmentSetup (InvestmentSetup, prices, wallet),
                                  Wallet (Wallet, amount))
import           Data.SortedList
import           Data.Time       (CalendarDiffDays (cdDays, cdMonths))
import qualified Money
import           Prelude         hiding (dropWhile, map)
import Data.Time.Calendar (addDays, addGregorianMonthsClip)
import Money (dense')
import Data.Ratio ((%))

type DolarCostAveragingInterval = CalendarDiffDays
getContributions :: Ord a => DataSet a -> DolarCostAveragingInterval -> DataSet a
getContributions (DataSet{values=dataPoints}) interval =
    case uncons dataPoints of
      Nothing -> mempty
      Just (x, xs) ->
        let
            currentDate = day x
            nextDate = addDays (cdDays interval) $ addGregorianMonthsClip (cdMonths interval) currentDate
            rest = dropWhile ((< nextDate) . day) xs
        in
            DataSet (singleton x) <> getContributions (DataSet rest) interval

buy :: Money.Dense "USD" -> DataPoint ClosePrice -> DataPoint BoughtUnits
buy balance dp =
    let
       cpR = toRational $ closePrice $ value dp
       aR = toRational balance
       shares = aR / cpR
    in DataPoint (day dp) (BoughtUnits shares)

dolarCostAveraging :: InvestmentSetup -> DolarCostAveragingInterval -> SortedList (DataPoint BoughtUnits)
dolarCostAveraging (InvestmentSetup{prices=priceData,wallet=Wallet{amount=moneyAmount}}) interval =
    let
        contributions = getContributions priceData interval
        contributionsCount = toInteger $ length $ values contributions
        periodContribution = dense' (1 % contributionsCount) * moneyAmount
    in
        map (buy periodContribution) (values priceData)

