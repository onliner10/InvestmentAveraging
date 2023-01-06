{-# OPTIONS_GHC -Wno-orphans #-}
module TimeArbitraries where

import           Control.Monad
import           Data.Time
import           Test.QuickCheck (Arbitrary (arbitrary), choose)

instance Arbitrary DayOfWeek where
    arbitrary = toEnum <$> choose (1,7)

instance Arbitrary Day where
    arbitrary = ModifiedJulianDay <$> choose (-313698, 2973483) -- 1000-01-1 to 9999-12-31

instance Arbitrary CalendarDiffDays where
    arbitrary = liftM2 CalendarDiffDays arbitrary arbitrary

