{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Core (DataPoint (DataPoint, value, day), ParseResult, DataSet (DataSet, values), ClosePrice (ClosePrice, closePrice), BoughtUnits (BoughtUnits), InvestmentSetup (InvestmentSetup, prices, wallet), Wallet(Wallet, amount)) where

import           Data.Ratio
import           Data.SortedList (SortedList)
import           Data.Time       (Day)
import           Data.Vector     (Vector)
import qualified Money
import           Prelude         hiding (dropWhile, map)

newtype ClosePrice = ClosePrice {closePrice :: Money.Dense "USD"} deriving (Eq, Show, Ord)

newtype BoughtUnits = BoughtUnits {getBoughtUnits :: Rational} deriving (Eq, Show, Ord)

data DataPoint a = DataPoint {day :: Day, value :: a} deriving (Show, Eq, Ord)

type ParseResult = Either String (Vector (DataPoint ClosePrice))

newtype DataSet a = DataSet {values :: SortedList (DataPoint a)} deriving (Show, Eq, Ord, Semigroup, Monoid)

newtype Wallet = Wallet {amount :: Money.Dense "USD"}

data InvestmentSetup = InvestmentSetup {prices :: DataSet ClosePrice, wallet :: Wallet}

