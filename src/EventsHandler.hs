module EventsHandler
  ( handleEvent
  , waitAnyKey
  ) where

import           Data.Char                 (isDigit)
import           Data.Foldable             (maximum)
import qualified Graphics.Gloss                   as Gloss
import           Graphics.Gloss.Data.Point (pointInBox)
import qualified Graphics.Gloss.Interface.IO.Game as Game

import Board        (Board (..), Player (..))
import PlayerAction (PlayerAction (..))
import RenderUtils  (betButtonX, buttonWidth, buttonHeight, checkButtonX, controlPanelY, foldButtonX)

handleEvent :: Game.Event -> Board -> (Board, Maybe PlayerAction)
handleEvent (Game.EventKey (Game.Char c)                       Game.Down _ _  ) board = handleCharKey c board
handleEvent (Game.EventKey (Game.SpecialKey Game.KeyPad0)      Game.Down _ _  ) board = handleCharKey '0' board
handleEvent (Game.EventKey (Game.SpecialKey Game.KeyPad1)      Game.Down _ _  ) board = handleCharKey '1' board
handleEvent (Game.EventKey (Game.SpecialKey Game.KeyPad2)      Game.Down _ _  ) board = handleCharKey '2' board
handleEvent (Game.EventKey (Game.SpecialKey Game.KeyPad3)      Game.Down _ _  ) board = handleCharKey '3' board
handleEvent (Game.EventKey (Game.SpecialKey Game.KeyPad4)      Game.Down _ _  ) board = handleCharKey '4' board
handleEvent (Game.EventKey (Game.SpecialKey Game.KeyPad5)      Game.Down _ _  ) board = handleCharKey '5' board
handleEvent (Game.EventKey (Game.SpecialKey Game.KeyPad6)      Game.Down _ _  ) board = handleCharKey '6' board
handleEvent (Game.EventKey (Game.SpecialKey Game.KeyPad7)      Game.Down _ _  ) board = handleCharKey '7' board
handleEvent (Game.EventKey (Game.SpecialKey Game.KeyPad8)      Game.Down _ _  ) board = handleCharKey '8' board
handleEvent (Game.EventKey (Game.SpecialKey Game.KeyPad9)      Game.Down _ _  ) board = handleCharKey '9' board
handleEvent (Game.EventKey (Game.SpecialKey Game.KeyDelete)    Game.Down _ _  ) board = deleteCurrentBet board
handleEvent (Game.EventKey (Game.SpecialKey Game.KeyBackspace) Game.Down _ _  ) board = deleteCurrentBet board
handleEvent (Game.EventKey (Game.MouseButton Game.LeftButton)  Game.Down _ pos) board = handleMouseButton pos board
handleEvent _                                                                   board = (board, Nothing)

deleteCurrentBet :: Board -> (Board, Maybe PlayerAction)
deleteCurrentBet board = (board { currentBet = currentBet board `div` 10 }, Nothing)

handleCharKey :: Char -> Board -> (Board, Maybe PlayerAction)
handleCharKey c board
  | c == 'f'  = (board, Just Fold)
  | c == 'b'  = (board, Just Bet)
  | c == 'c'  = (board, Just Check)
  | isDigit c = (board { currentBet = 10 * (currentBet board) + read [c] }, Nothing)
  | otherwise = (board, Nothing)

handleMouseButton :: Gloss.Point -> Board -> (Board, Maybe PlayerAction)
handleMouseButton pos board
  | uncurry (pointInBox pos) (getButtonRect betButtonX)    = handleCharKey 'b' board
  | uncurry (pointInBox pos) (getButtonRect checkButtonX)  = handleCharKey 'c' board
  | uncurry (pointInBox pos) (getButtonRect foldButtonX)   = handleCharKey 'f' board
  | otherwise                                              = handleCharKey '_' board

getButtonRect :: Float -> (Gloss.Point, Gloss.Point)
getButtonRect xDiff = (p1, p3)
  where
    addToPair (x, y) = (x + xDiff, y + controlPanelY)
    [p1, p2, p3, p4] = map addToPair $ Game.rectanglePath buttonWidth buttonHeight

waitAnyKey :: Game.Event -> Board -> (Board, Maybe PlayerAction)
waitAnyKey (Game.EventKey (Game.SpecialKey _) _ _ _) board = (board { needAnyKey = False }, Just Ok)
waitAnyKey (Game.EventKey (Game.Char       _) _ _ _) board = (board { needAnyKey = False }, Just Ok)
waitAnyKey _                                         board = (board, Nothing)
