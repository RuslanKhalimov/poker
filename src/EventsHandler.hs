module EventsHandler
  ( handleEvent
  , waitAnyKey
  ) where

import           Control.Lens              ((^.), (.~), (%~), (.=), (%=), (+=), (*=))
import           Control.Monad.State       (State, execState, gets)
import           Data.Char                 (isDigit)
import           Data.Foldable             (maximum)
import qualified Graphics.Gloss                   as Gloss
import           Graphics.Gloss.Data.Point (pointInBox)
import qualified Graphics.Gloss.Interface.IO.Game as Game

import Board        (Board, Player, currentBet, needAnyKey)
import PlayerAction (PlayerAction (..))
import RenderUtils  (betButtonX, buttonWidth, buttonHeight, checkButtonX, controlPanelY, foldButtonX)

handleEvent :: Game.Event -> State Board (Maybe PlayerAction)
handleEvent (Game.EventKey (Game.Char c)                       Game.Down _ _  ) = handleCharKey c
handleEvent (Game.EventKey (Game.SpecialKey Game.KeyPad0)      Game.Down _ _  ) = handleCharKey '0'
handleEvent (Game.EventKey (Game.SpecialKey Game.KeyPad1)      Game.Down _ _  ) = handleCharKey '1'
handleEvent (Game.EventKey (Game.SpecialKey Game.KeyPad2)      Game.Down _ _  ) = handleCharKey '2'
handleEvent (Game.EventKey (Game.SpecialKey Game.KeyPad3)      Game.Down _ _  ) = handleCharKey '3'
handleEvent (Game.EventKey (Game.SpecialKey Game.KeyPad4)      Game.Down _ _  ) = handleCharKey '4'
handleEvent (Game.EventKey (Game.SpecialKey Game.KeyPad5)      Game.Down _ _  ) = handleCharKey '5'
handleEvent (Game.EventKey (Game.SpecialKey Game.KeyPad6)      Game.Down _ _  ) = handleCharKey '6'
handleEvent (Game.EventKey (Game.SpecialKey Game.KeyPad7)      Game.Down _ _  ) = handleCharKey '7'
handleEvent (Game.EventKey (Game.SpecialKey Game.KeyPad8)      Game.Down _ _  ) = handleCharKey '8'
handleEvent (Game.EventKey (Game.SpecialKey Game.KeyPad9)      Game.Down _ _  ) = handleCharKey '9'
handleEvent (Game.EventKey (Game.SpecialKey Game.KeyDelete)    Game.Down _ _  ) = deleteCurrentBet
handleEvent (Game.EventKey (Game.SpecialKey Game.KeyBackspace) Game.Down _ _  ) = deleteCurrentBet
handleEvent (Game.EventKey (Game.MouseButton Game.LeftButton)  Game.Down _ pos) = handleMouseButton pos
handleEvent _                                                                   = return Nothing

deleteCurrentBet :: State Board (Maybe PlayerAction)
deleteCurrentBet = do
  currentBet %= (`div` 10)
  return Nothing

handleCharKey :: Char -> State Board (Maybe PlayerAction)
handleCharKey c
  | c == 'f'  = return $ Just Fold
  | c == 'b'  = gets (Just . Bet . (^.currentBet))
  | c == 'c'  = return $ Just Check
  | isDigit c = do
    currentBet *= 10
    currentBet += read [c]
    return Nothing
  | otherwise = return Nothing

handleMouseButton :: Gloss.Point -> State Board (Maybe PlayerAction)
handleMouseButton pos
  | uncurry (pointInBox pos) (getButtonRect betButtonX)    = handleCharKey 'b'
  | uncurry (pointInBox pos) (getButtonRect checkButtonX)  = handleCharKey 'c'
  | uncurry (pointInBox pos) (getButtonRect foldButtonX)   = handleCharKey 'f'
  | otherwise                                              = handleCharKey '_'

getButtonRect :: Float -> (Gloss.Point, Gloss.Point)
getButtonRect xDiff = (p1, p3)
  where
    addToPair (x, y) = (x + xDiff, y + controlPanelY)
    [p1, p2, p3, p4] = map addToPair $ Game.rectanglePath buttonWidth buttonHeight

waitAnyKey :: Game.Event -> State Board (Maybe PlayerAction)
waitAnyKey (Game.EventKey (Game.SpecialKey _) _ _ _) = do
  needAnyKey .= False
  return $ Just Ok
waitAnyKey (Game.EventKey (Game.Char       _) _ _ _) = do
  needAnyKey .= False
  return $ Just Ok
waitAnyKey _                                         = return Nothing
