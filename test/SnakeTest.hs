{-# LANGUAGE TemplateHaskell #-}

module SnakeTest where

import Control.Monad.State
import qualified Data.Sequence as Seq
import FoodTest
import Linear.V2 (V2 (..))
import Snake
import System.Random (Random (..), StdGen, newStdGen)
import qualified System.Random as Rand
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)

-- | Determine if two directions are opposites
areOpposite :: Direction -> Direction -> Bool
areOpposite North South = True
areOpposite South North = True
areOpposite East West = True
areOpposite West East = True
areOpposite _ _ = False

-- | Test property for 'turn' function
prop_turnUpdatesDirection :: Property
prop_turnUpdatesDirection = forAll (elements directions) $ \(initialDir, newDir) ->
  monadicIO $ do
    let seed = Rand.mkStdGen 0
    let initialState = (initGameWithSeed seed) {_dir = initialDir}
    let newState = turn newDir initialState

    -- Skip assertion if the new direction is opposite the initial direction
    if areOpposite initialDir newDir
      then return ()
      else assert $ _dir newState == newDir
  where
    directions = [(d1, d2) | d1 <- [North, South, East, West], d2 <- [North, South, East, West]]
