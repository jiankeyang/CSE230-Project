{-# LANGUAGE OverloadedStrings #-}

module UI where

import Brick
  ( App (..),
    AttrMap,
    AttrName,
    BrickEvent (..),
    EventM,
    Next,
    Padding (..),
    Widget,
    attrMap,
    continue,
    customMain,
    emptyWidget,
    fg,
    hBox,
    hLimit,
    halt,
    neverShowCursor,
    on,
    padAll,
    padLeft,
    padRight,
    padTop,
    str,
    vBox,
    vLimit,
    withAttr,
    withBorderStyle,
    (<+>),
  )
import Brick.BChan (BChan, newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Control.Concurrent (forkIO, threadDelay)
import Control.Lens ((^.))
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import qualified Graphics.Vty as V
import Linear.V2 (V2 (..))
import Snake

-- Types

-- | Ticks mark passing of time
--
-- This is our custom event that will be constantly fed into the app.
data Tick = Tick

-- | Named resources
--
-- Not currently used, but will be easier to refactor
-- if we call this "Name" now.
type Name = ()

data Cell = Snake | Food | Barrier | Empty

-- App definition

app :: App Game Tick Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return,
      appAttrMap = const theMap
    }

main :: IO ()
main = do
  chan <- newBChan 10
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  -- _ <- customMain initialVty builder Nothing coverApp ()
  forkIO $
    forever $ do
      writeBChan chan Tick
      threadDelay 10000 -- decides how fast your game moves
  g <- initGame
  void $ customMain initialVty builder (Just chan) app g

-- Handling events

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g event = case _gameState g of
  Cover -> handleCoverEvent g event
  Playing -> handlePlayingEvent g event

handleCoverEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleCoverEvent g (VtyEvent (V.EvKey _ _)) = continue $ g {_gameState = Playing}
handleCoverEvent g _ = continue g

handlePlayingEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handlePlayingEvent g (AppEvent Tick) = continue $ step g
handlePlayingEvent g (VtyEvent (V.EvKey V.KUp [])) = continue $ turn North g
handlePlayingEvent g (VtyEvent (V.EvKey V.KDown [])) = continue $ turn South g
handlePlayingEvent g (VtyEvent (V.EvKey V.KRight [])) = continue $ turn East g
handlePlayingEvent g (VtyEvent (V.EvKey V.KLeft [])) = continue $ turn West g
handlePlayingEvent g (VtyEvent (V.EvKey (V.KChar 'k') [])) = continue $ turn North g
handlePlayingEvent g (VtyEvent (V.EvKey (V.KChar 'j') [])) = continue $ turn South g
handlePlayingEvent g (VtyEvent (V.EvKey (V.KChar 'l') [])) = continue $ turn East g
handlePlayingEvent g (VtyEvent (V.EvKey (V.KChar 'h') [])) = continue $ turn West g
handlePlayingEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO (initGame) >>= continue
handlePlayingEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handlePlayingEvent g (VtyEvent (V.EvKey V.KEsc [])) = halt g
handlePlayingEvent g _ = continue g

-- Drawing

drawUI :: Game -> [Widget Name]
drawUI g = case _gameState g of
  Cover -> [drawCover]
  Playing -> [C.center $ padRight (Pad 2) (drawStats g) <+> drawGrid g]

drawStats :: Game -> Widget Name
drawStats g =
  hLimit 18 $
    vBox
      [ drawScore (g ^. score),
        padTop (Pad 2) $ drawClock (g ^. timer),
        padTop (Pad 2) $ drawTips,
        padTop (Pad 0) $ drawGameOver (g ^. dead)
      ]

drawCover :: Widget Name
drawCover =
  C.center $
    withBorderStyle BS.unicodeBold $
      vBox
        [ withAttr "redAttr" $ C.hCenter $ str " SSSSS  N   N  AAAAA  K   K  EEEEE",
          withAttr "yellowAttr" $ C.hCenter $ str " SS     NN  N  A   A  K  K   E    ",
          withAttr "greenAttr" $ C.hCenter $ str "   S    N N N  AAAAA  KKK    EEEE ",
          withAttr "blueAttr" $ C.hCenter $ str "    SS  N  NN  A   A  K  K   E    ",
          withAttr "magentaAttr" $ C.hCenter $ str " SSSSS  N   N  A   A  K   K  EEEEE",
          C.hCenter $ padTop (Pad 2) $ str "Press any key to start"
        ]

drawScore :: Int -> Widget Name
drawScore n =
  withBorderStyle BS.unicodeBold $
    B.borderWithLabel (str "Score") $
      C.hCenter $
        padAll 1 $
          str $ show n

drawGameOver :: Bool -> Widget Name
drawGameOver dead =
  if dead
    then withAttr gameOverAttr $ C.hCenter $ str "GAME OVER"
    else emptyWidget

drawGrid :: Game -> Widget Name
drawGrid g =
  withBorderStyle BS.unicodeBold $
    B.borderWithLabel (str "Snake") $
      vBox rows
  where
    rows = [hBox $ cellsInRow r | r <- [height -1, height -2 .. 0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0 .. width -1]]
    drawCoord = drawCell . cellAt
    cellAt c
      | c `elem` g ^. snake = Snake
      | c `elem` g ^. barrier = Barrier
      | c == g ^. food = Food
      | otherwise = Empty

drawCell :: Cell -> Widget Name
drawCell Snake = withAttr snakeAttr cw
drawCell Food = withAttr foodAttr cw
drawCell Barrier = withAttr barrierAttr cw
drawCell Empty = withAttr emptyAttr cw

drawClock :: Int -> Widget Name
drawClock seconds =
  withBorderStyle BS.unicodeBold $
    B.borderWithLabel (str "Time") $
      C.hCenter $
        padAll 1 $
          str $ formatTime seconds

formatTime :: Int -> String
formatTime seconds =
  let (minutes, remainingSeconds) = divMod seconds 60
      paddedMinutes = padZero 2 minutes
      paddedSeconds = padZero 2 remainingSeconds
   in paddedMinutes ++ ":" ++ paddedSeconds

padZero :: Int -> Int -> String
padZero width number = replicate (width - length (show number)) '0' ++ show number

drawTips =
  withBorderStyle BS.unicodeBold $
    B.borderWithLabel (str "Tips") $ -- Optional: Add a border with label
      C.hCenter $
        padAll 1 $
          vBox
            [ str " r: restart",
              str " q: quit",
              str " arrows: move"
            ]

cw :: Widget Name
cw = str "  "

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (snakeAttr, V.blue `on` V.blue),
      (foodAttr, V.red `on` V.red),
      (gameOverAttr, fg V.red `V.withStyle` V.bold),
      (barrierAttr, V.black `on` V.black),
      -- Rainbow color attributes
      ("redAttr", fg V.red),
      ("yellowAttr", fg V.yellow),
      ("greenAttr", fg V.green),
      ("blueAttr", fg V.blue),
      ("magentaAttr", fg V.magenta)
    ]

gameOverAttr :: AttrName
gameOverAttr = "gameOver"

snakeAttr, foodAttr, emptyAttr, barrierAttr :: AttrName
snakeAttr = "snakeAttr"
foodAttr = "foodAttr"
emptyAttr = "emptyAttr"
barrierAttr = "barrierAttr"
