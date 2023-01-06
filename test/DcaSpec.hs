module DcaSpec where
import Core (InvestmentSetup)
import Dca (DolarCostAveragingInterval)

instance Arbitrary (InvestmentSetup) where


prop_contributorsSumsUpToWallet :: InvestmentSetup -> DolarCostAveragingInterval -> Bool
prop_contributorsSumsUpToWallet = undefined
