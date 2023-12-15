import FoodTest
import SnakeTest
import System.Random (StdGen, newStdGen, randomR)
import Test.QuickCheck

-- | Main function to run all tests
main :: IO ()
main = do
  putStrLn "Testing nextFood placement"
  quickCheck prop_nextFoodPlacement
  putStrLn "Testing turn updates direction"
  quickCheck prop_turnUpdatesDirection
