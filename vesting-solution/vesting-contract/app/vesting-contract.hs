import           Prelude
import           Cardano.Api
import           VestingContract ( vestingContractScript )

main :: IO ()
main = do
  result <- writeFileTextEnvelope "vesting-contract.plutus" Nothing vestingContractScript
  case result of
    Left err -> print $ displayError err
    Right () -> return ()
