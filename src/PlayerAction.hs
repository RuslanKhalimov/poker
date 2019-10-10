{-# LANGUAGE DeriveGeneric #-}

module PlayerAction
  ( PlayerAction (..)
  , bet
  , check
  , foldCards
  ) where

import Data.Binary  (Binary)
import GHC.Generics (Generic)

import Board      (Board (..), Player (..))
import BoardUtils (getFromActivePlayer, getMaxBet, modifyActivePlayer)

data PlayerAction = Fold
                  | Bet
                  | Check
                  | Ok
  deriving (Generic, Show)

instance Binary PlayerAction where

makeBet :: Board -> Board
makeBet board = modifyActivePlayer mapper $ board { needAction   = False
                                                  , currentBet   = 0
                                                  , stepsInRound = stepsInRound board + 1
                                                  }
  where
    mapper player = player { playerBet   = playerBet player + min (currentBet board) (playerMoney player)
                           , playerMoney = max 0 (playerMoney player - currentBet board)
                           }

foldCards :: Board -> Board
foldCards board = makeBet $ modifyActivePlayer mapper board { currentBet = 0 }
  where
    mapper player = player { isInGame = False }

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
