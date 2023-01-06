{-# LANGUAGE DataKinds #-}
module Dca (dolarCostAveraging, DcaInterval (DcaInterval)) where
import           Core               (BoughtUnits (BoughtUnits),
                                     ClosePrice (closePrice),
                                     DataPoint (DataPoint, day, value),
                                     DataSet (DataSet, values),
                                     InvestmentSetup (InvestmentSetup, prices, wallet),
                                     Wallet (Wallet, amount))
import           Data.Ratio         ((%))
import           Data.SortedList
import           Data.Time          (CalendarDiffDays (cdDays, cdMonths))
import           Data.Time.Calendar (addDays, addGregorianMonthsClip)
import           Debug.Trace
import           Money              (dense')
import qualified Money
import           Prelude            hiding (dropWhile, map)

-- TODO: Verify it is greater than zero
newtype DcaInterval = DcaInterval { getInterval :: CalendarDiffDays} deriving (Show, Eq)
getContributions :: Ord a => DataSet a -> DcaInterval -> DataSet a
getContributions (DataSet{values=prices}) dcaInterval =
    case uncons prices of
      Nothing -> mempty
      Just (x, xs) ->
        let
            (DcaInterval{getInterval=interval}) = dcaInterval
            currentDate = day x
            nextDate = addDays (cdDays interval) $ addGregorianMonthsClip (cdMonths interval) currentDate
            rest = dropWhile ((< nextDate) . day) xs
        in
            DataSet (singleton x) <> getContributions (DataSet rest) dcaInterval

buy :: Money.Dense "USD" -> DataPoint ClosePrice -> DataPoint BoughtUnits
buy balance dp =
    let
       cpR = toRational $ closePrice $ value dp
       aR = toRational balance
       shares = aR / cpR
    in DataPoint (day dp) (BoughtUnits shares)

dolarCostAveraging :: InvestmentSetup -> DcaInterval -> SortedList (DataPoint BoughtUnits)
dolarCostAveraging (InvestmentSetup{prices=priceData,wallet=Wallet{amount=moneyAmount}}) interval =
    let
        contributions = trace "Contributions" $ traceShowId $ getContributions priceData interval
        contributionsCount = traceShowId $ toInteger $ length $ values contributions
        periodContribution = dense' (1 % contributionsCount) * moneyAmount
    in
        map (buy periodContribution) (values priceData)

