{-# LANGUAGE DeriveGeneric #-}

module PlayerAction
  ( PlayerAction (..)
  , bet
  , check
  , foldCards
  , quit
  ) where

import           Data.Binary  (Binary)
import qualified Data.Map as Map
import           GHC.Generics (Generic)

import Board      (Board (..), Player (..))
import BoardUtils (addBet, getFromActivePlayer, getMaxBet, modifyActivePlayer)

data PlayerAction = Fold
                  | Bet Int
                  | Check
                  | Ok
                  | Quit
  deriving (Generic, Show)

instance Binary PlayerAction where

makeBet :: Board -> Board
makeBet board = modifyActivePlayer mapper $ board { needAction   = False
                                                  , currentBet   = 0
                                                  , stepsInRound = stepsInRound board + 1
                                                  }
  where
    mapper :: Player -> Player
    mapper p = addBet (currentBet board `min` playerMoney p) p

foldCards :: Board -> Board
foldCards board = makeBet $ modifyActivePlayer mapper board { currentBet   = 0
                                                            , stepsInRound = stepsInRound board - 1
                                                            }
  where
    mapper player = player { isInGame = False }

bet :: Int -> Board -> Board
bet count board = makeBet board { currentBet   = _currentBet
                                , stepsInRound = if _currentBet + getFromActivePlayer playerBet board > getMaxBet board
                                                 then
                                                   0
                                                 else
                                                   stepsInRound board
                                }
  where
    _currentBet = count `max` getMaxBet board - getFromActivePlayer playerBet board

check :: Board -> Board
check board = bet 0 board

quit :: Board -> Board
quit board = board { players      = Map.delete (activePlayerId board) (players board)
                   , playersCount = playersCount board - 1
                   }
