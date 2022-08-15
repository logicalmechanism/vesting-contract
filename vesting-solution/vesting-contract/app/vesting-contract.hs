import Prelude
import Cardano.Api
import VestingContract ( vestingContractScript )
{- |
  Author   : The Ancient Kraken
  Copyright: 2022
-}
main :: IO ()
main = do
  result <- writeFileTextEnvelope "vesting-contract.plutus" Nothing vestingContractScript
  case result of
    Left err -> print $ displayError err
    Right () -> return ()
