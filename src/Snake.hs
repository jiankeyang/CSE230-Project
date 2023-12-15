{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Snake
  ( initGame,
    step,
    turn,
    Game (..),
    Direction (..),
    GameLevel (..),
    dead,
    food,
    score,
    speedLevel,
    snake,
    glevels,
    height,
    width,
    barrier,
    timer,
    paused,
    tickCount,
    currentLevel,
  )
where

import Control.Applicative ((<|>))
import Control.Lens hiding ((:<), (:>), (<|), (|>))
import Control.Monad (guard, when)
import Control.Monad.Extra (orM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq (..), (<|))
import qualified Data.Sequence as S
import Linear.V2 (V2 (..), _x, _y)
import System.Random (Random (..), newStdGen)
import Data.List (find)

-- Types

data Game = Game
  { -- | snake as a sequence of points in N2
    _snake :: Snake,
    -- | direction
    _dir :: Direction,
    -- | location of the food
    _food :: Coord,
    -- | infinite list of random next food locations
    _foods :: Stream Coord,
    -- | game over flag
    _dead :: Bool,
    -- | paused flag
    _paused :: Bool,
    -- | score
    _score :: Int,
    -- | speed
    _speedLevel :: Int,
    -- | tick count
    _tickCount :: Int,
    -- | lock to disallow duplicate turns between time steps
    _locked :: Bool,
    -- barrier
    _barrier :: [Coord],
    -- remaining time in seconds
    _timer :: Int,
    -- the game level player is in
    _currentLevel :: GameLevel
  }
  deriving (Show)

type Coord = V2 Int

type Snake = Seq Coord

data Stream a = a :| Stream a
  deriving (Show)

data Direction
  = North
  | South
  | East
  | West
  deriving (Eq, Show)
height, width :: Int
height = 20
width = 20

makeSquareBarrier :: Coord -> [Coord]
makeSquareBarrier (V2 x y) = [V2 (x + dx) (y + dy) | dx <- [0 .. 2], dy <- [0 .. 2], dx == 0 || dx == 2 || dy == 0 || dy == 2]

squareBarriers :: [Coord]
squareBarriers = concatMap makeSquareBarrier squareTops
  where
    squareTops =
      [ V2 (width `div` 4 - 1) (height `div` 4 - 1),
        V2 (3 * width `div` 4 - 2) (height `div` 4 - 1),
        V2 (width `div` 4 - 1) (3 * height `div` 4 - 2),
        V2 (3 * width `div` 4 - 2) (3 * height `div` 4 - 2)
      ]
borderBarrier :: [Coord]
borderBarrier = topBottomBorder ++ leftRightBorder
  where
    gapSize = 6 -- The size of the opening in the center
    gapStart = width `div` 2 - gapSize `div` 2

    -- Top and Bottom borders with openings
    topBottomBorder =
      [V2 x 0 | x <- [0 .. width - 1], not (x >= gapStart && x < gapStart + gapSize)]
        ++ [V2 x (height - 1) | x <- [0 .. width - 1], not (x >= gapStart && x < gapStart + gapSize)]

    -- Left and Right borders with openings
    leftRightBorder =
      [V2 0 y | y <- [1 .. height - 2], not (y >= gapStart && y < gapStart + gapSize)]
        ++ [V2 (width - 1) y | y <- [1 .. height - 2], not (y >= gapStart && y < gapStart + gapSize)]
data GameLevel = GameLevel
  { levelId :: Int,
    scoreThreshold :: Int,
    barrierLayout :: [Coord]
  } deriving (Show)
level1 :: GameLevel
level1 = GameLevel 1 0 borderBarrier

level2 :: GameLevel
level2 = GameLevel 2 50 squareBarriers -- Define `anotherBarrierPattern` for this level

glevels :: [GameLevel]
glevels = [level1, level2]

makeLenses ''Game

-- Constants

-- height, width :: Int
-- height = 20
-- width = 20

squareCenters :: [Coord]
squareCenters =
  [ V2 5 5, -- Center of the first square
    V2 15 5, -- Center of the second square
    V2 5 15, -- Center of the third square
    V2 15 15 -- Center of the fourth square
  ]

-- Functions

-- | Step forward in time
step :: Game -> Game
step s = flip execState s . runMaybeT $ do
  -- Increase tick count
  MaybeT . fmap Just $ tickCount += 1

  -- Check for speed level
  currentTickCount <- lift $ use tickCount
  currentSpeedLevel <- lift $ use speedLevel
  guard (currentTickCount `mod` currentSpeedLevel == 0)
  -- Make sure the game isn't paused or over
  MaybeT $ guard . not <$> orM [use paused, use dead]

  -- Unlock from last directional turn
  MaybeT . fmap Just $ locked .= False

  -- timer count down
  -- guard (currentTickCount `mod` 10 == 0)
  -- MaybeT . fmap Just $ timer %= (\t -> max 0 (t - 1))
  let shouldUpdateTimer = currentTickCount `mod` 100 == 0
  if shouldUpdateTimer
    then timer %= (\t -> max 0 (t - 1))
    else return ()

  -- die (moved into boundary), eat (moved into food), or move (move into space)
  die <|> eatFood <|> timeUp <|> MaybeT (Just <$> modify move)

  currentScore <- lift $ use score
  let newLevel = last . takeWhile (\lvl -> currentScore >= scoreThreshold lvl) $ glevels
  currentLevelValue <- lift $ use currentLevel
  let currentLevelId = levelId currentLevelValue
  let newLevelId = levelId newLevel

  when (newLevelId /= currentLevelId) $ lift $ do
    currentLevel .= newLevel
    barrier .= barrierLayout newLevel

-- | Possibly die if next head position is in snake
die :: MaybeT (State Game) ()
die = do
  nh <- lift $ nextHead <$> get
  snakeBody <- lift $ use snake
  barrierPositions <- lift $ use barrier
  let isCollision = nh `elem` snakeBody || nh `elem` barrierPositions
  guard isCollision
  lift $ dead .= True

-- | Possibly eat food if next head position is food
eatFood :: MaybeT (State Game) ()
eatFood = do
  MaybeT . fmap guard $ (==) <$> (nextHead <$> get) <*> (use food)
  MaybeT . fmap Just $ do
    modifying score (+ 10)
    g <- get
    let newSpeed =
          if g ^. score `mod` 50 == 0 && g ^. speedLevel > 1
            then (g ^. speedLevel) - 1
            else g ^. speedLevel
    speedLevel .= newSpeed
    modifying snake (nextHead g <|)
    nextFood

-- | Set a valid next food coordinate
nextFood :: State Game ()
nextFood = do
  (f :| fs) <- use foods
  foods .= fs
  snakeBody <- use snake
  barriers <- use barrier
  let isInvalidLocation coord = coord `elem` snakeBody || coord `elem` barriers || coord `elem` squareCenters
  findValidFoodLocation f fs isInvalidLocation

findValidFoodLocation :: Coord -> Stream Coord -> (Coord -> Bool) -> State Game ()
findValidFoodLocation currentFood foodStream isInvalid =
  if isInvalid currentFood
    then case foodStream of
      (nextFood :| rest) -> findValidFoodLocation nextFood rest isInvalid
    else food .= currentFood

timeUp :: MaybeT (State Game) ()
timeUp = do
  currentTimer <- lift $ use timer
  guard (currentTimer == 0)
  lift $ dead .= True

-- | Move snake along in a marquee fashion
move :: Game -> Game
move g@Game {_snake = (s :|> _)} = g & snake .~ (nextHead g <| s)
move _ = error "Snakes can't be empty!"

-- | Get next head position of the snake
nextHead :: Game -> Coord
nextHead Game {_dir = d, _snake = (a :<| _)}
  | d == North = a & _y %~ (\y -> (y + 1) `mod` height)
  | d == South = a & _y %~ (\y -> (y - 1) `mod` height)
  | d == East = a & _x %~ (\x -> (x + 1) `mod` width)
  | d == West = a & _x %~ (\x -> (x - 1) `mod` width)
nextHead _ = error "Snakes can't be empty!"

-- | Turn game direction (only turns orthogonally)
--
-- Implicitly unpauses yet locks game
turn :: Direction -> Game -> Game
turn d g =
  if g ^. locked
    then g
    else g & dir %~ turnDir d & paused .~ False & locked .~ True

turnDir :: Direction -> Direction -> Direction
turnDir n c
  | c `elem` [North, South] && n `elem` [East, West] = n
  | c `elem` [East, West] && n `elem` [North, South] = n
  | otherwise = c

-- makeSquareBarrier :: Coord -> [Coord]
-- makeSquareBarrier (V2 x y) = [V2 (x + dx) (y + dy) | dx <- [0 .. 2], dy <- [0 .. 2], dx == 0 || dx == 2 || dy == 0 || dy == 2]

-- | Initialize a paused game with random food location
initGame :: IO Game
initGame = do
  (f :| fs) <-
    fromList . randomRs (V2 0 0, V2 (width - 1) (height - 1)) <$> newStdGen

  let xm = width `div` 2
      ym = height `div` 2
      -- squareBarriers :: [Coord]
      -- squareBarriers = concatMap makeSquareBarrier squareTops
      --   where
      --     squareTops =
      --       [ V2 (width `div` 4 - 1) (height `div` 4 - 1),
      --         V2 (3 * width `div` 4 - 2) (height `div` 4 - 1),
      --         V2 (width `div` 4 - 1) (3 * height `div` 4 - 2),
      --         V2 (3 * width `div` 4 - 2) (3 * height `div` 4 - 2)
      --       ]
      -- borderBarrier :: [Coord]
      -- borderBarrier = topBottomBorder ++ leftRightBorder
      --   where
      --     gapSize = 6 -- The size of the opening in the center
      --     gapStart = width `div` 2 - gapSize `div` 2

      --     -- Top and Bottom borders with openings
      --     topBottomBorder =
      --       [V2 x 0 | x <- [0 .. width - 1], not (x >= gapStart && x < gapStart + gapSize)]
      --         ++ [V2 x (height - 1) | x <- [0 .. width - 1], not (x >= gapStart && x < gapStart + gapSize)]

      --     -- Left and Right borders with openings
      --     leftRightBorder =
      --       [V2 0 y | y <- [1 .. height - 2], not (y >= gapStart && y < gapStart + gapSize)]
      --         ++ [V2 (width - 1) y | y <- [1 .. height - 2], not (y >= gapStart && y < gapStart + gapSize)]

      mazePositions = squareBarriers ++ borderBarrier
      g =
        Game
          { _snake = S.singleton (V2 xm ym),
            _food = f,
            _foods = fs,
            _score = 0,
            _speedLevel = 10,
            _tickCount = 0,
            _dir = North,
            _dead = False,
            _paused = True,
            _locked = False,
            _barrier = mazePositions,
            _timer = 20, -- Could be manually modified
            _currentLevel = head glevels
          }
  return $ execState nextFood g

fromList :: [a] -> Stream a
fromList = foldr (:|) (error "Streams must be infinite")
