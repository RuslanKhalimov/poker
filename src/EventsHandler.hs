module EventsHandler
  ( bet
  , check
  , foldCards
  , handleEvent
  , waitAnyKey
  ) where

import           Data.Char                 (isDigit)
import           Data.Foldable             (maximum)
import qualified Graphics.Gloss                   as Gloss
import           Graphics.Gloss.Data.Point (pointInBox)
import qualified Graphics.Gloss.Interface.IO.Game as Game

import Board       (Board (..), Player (..))
import BoardUtils  (getFromActivePlayer, getMaxBet, modifyActivePlayer)
import RenderUtils (betButtonX, buttonWidth, buttonHeight, checkButtonX, controlPanelY, foldButtonX)

foldCards :: Board -> Board
foldCards board = makeBet $ modifyActivePlayer mapper board { currentBet = 0 }
  where
    mapper player = player { isInGame = False }

makeBet :: Board -> Board
makeBet board = modifyActivePlayer mapper $ board { needAction   = False
                                                  , currentBet   = 0
                                                  , stepsInRound = stepsInRound board + 1
                                                  }
  where
    mapper player = player { playerBet   = min (playerBet player + currentBet board) (playerMoney player)
                           , playerMoney = max 0 (playerMoney player - currentBet board)
                           }

bet :: Board -> Board
bet board = makeBet board { currentBet   = max (currentBet board) (getMaxBet board - getFromActivePlayer playerBet board)
                          , stepsInRound = if currentBet board + getFromActivePlayer playerBet board > getMaxBet board
                                           then
                                             0
                                           else
                                             stepsInRound board
                          }

check :: Board -> Board
check board = bet board { currentBet = 0 }

handleEvent :: Game.Event -> Board -> Board
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
handleEvent _                                                                   board = board

deleteCurrentBet :: Board -> Board
deleteCurrentBet board = board { currentBet = currentBet board `div` 10 }

handleCharKey :: Char -> Board -> Board
handleCharKey c board
  | c == 'f'  = foldCards board
  | c == 'b'  = bet       board
  | c == 'c'  = check     board
  | isDigit c = board { currentBet = 10 * (currentBet board) + read [c] }
  | otherwise = board

handleMouseButton :: Gloss.Point -> Board -> Board
handleMouseButton pos board
  | uncurry (pointInBox pos) (getButtonRect betButtonX)    = bet       board
  | uncurry (pointInBox pos) (getButtonRect checkButtonX)  = check      board
  | uncurry (pointInBox pos) (getButtonRect foldButtonX)   = foldCards board
  | otherwise                                              = board

getButtonRect :: Float -> (Gloss.Point, Gloss.Point)
getButtonRect xDiff = (p1, p3)
  where
    addToPair (x, y) = (x + xDiff, y + controlPanelY)
    [p1, p2, p3, p4] = map addToPair $ Game.rectanglePath buttonWidth buttonHeight

waitAnyKey :: Game.Event -> Board -> Board
waitAnyKey (Game.EventKey (Game.SpecialKey _) _ _ _) board = board { needAnyKey = False }
waitAnyKey (Game.EventKey (Game.Char       _) _ _ _) board = board { needAnyKey = False }
waitAnyKey _                                         board = board
