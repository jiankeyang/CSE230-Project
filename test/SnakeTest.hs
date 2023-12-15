{-# LANGUAGE TemplateHaskell #-}

import Control.Monad.State
import qualified Data.Sequence as Seq
import Linear.V2 (V2 (..))
import Snake
import System.Random (Random (..), StdGen, newStdGen)
import qualified System.Random as Rand
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)

-- | Helper function to initialize the game with a specific seed
initGameWithSeed :: StdGen -> Game
initGameWithSeed seed =
  Game
    { _snake = Seq.singleton (V2 xm ym),
      _food = foodPosition,
      _foods = fs,
      _score = 0,
      _speedLevel = 10,
      _tickCount = 0,
      _dir = North,
      _dead = False,
      _paused = True,
      _locked = False,
      _barrier = mazePositions,
      _timer = 20 -- Could be manually modified
    }
  where
    initialSnakePosition = Seq.fromList [V2 5 5] -- Example initial position
    (foodPosition, newSeed) = randomR (V2 0 0, V2 (width - 1) (height - 1)) seed -- Example food generation
    fs = fromList $ randomRs (V2 0 0, V2 (width - 1) (height - 1)) newSeed
    mazePositions = squareBarriers ++ borderBarrier
    xm = width `div` 2
    ym = height `div` 2

-- | Test property for 'nextFood' function
prop_nextFoodPlacement :: Property
prop_nextFoodPlacement = monadicIO $ do
  -- Generate a new StdGen
  seed <- run newStdGen
  let initialState = initGameWithSeed seed
  let newState = execState nextFood initialState
  let mazePositions = squareBarriers ++ borderBarrier
  -- food location, it is a Coord
  let foodLoc = _food newState
  -- check if food is in a valid position
  assert $ foodLoc `notElem` mazePositions

-- | Test property for 'move' function
-- Add a similar structure for testing the 'move' function

-- | Test property for 'turn' function
-- Add a similar structure for testing the 'turn' function

-- | Main function to run all tests
main :: IO ()
main = do
  putStrLn "Testing nextFood placement"
  quickCheck prop_nextFoodPlacement
