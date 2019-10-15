{-# LANGUAGE DeriveGeneric #-}

module PlayerAction
  ( PlayerAction (..)
  , bet
  , check
  , foldCards
  , quit
  ) where

import           Control.Lens ((^.), (.~), (%~))
import           Data.Binary  (Binary)
import qualified Data.Map as Map
import           GHC.Generics (Generic)

import Board      ( Board, Player, activePlayerId, currentBet, isInGame, needAction, stepsInRound
                  , playerBet, playerMoney, players, playersCount)
import BoardUtils (addBet, getFromActivePlayer, getMaxBet, modifyActivePlayer)

data PlayerAction = Fold
                  | Bet Int
                  | Check
                  | Ok
                  | Quit
  deriving (Generic, Show)

instance Binary PlayerAction where

makeBet :: Board -> Board
makeBet board = modifyActivePlayer mapper $ needAction   .~ False
                                          $ currentBet   .~ 0
                                          $ stepsInRound %~ succ
                                          $ board
  where
    mapper :: Player -> Player
    mapper player = addBet ((board^.currentBet) `min` (player^.playerMoney)) player

foldCards :: Board -> Board
foldCards board = makeBet $ modifyActivePlayer (isInGame .~ False)
                          $ currentBet   .~ 0
                          $ stepsInRound %~ pred
                          $ board

bet :: Int -> Board -> Board
bet count board = makeBet $ currentBet   .~ newCurrentBet
                          $ stepsInRound .~ (if newCurrentBet + activePlayerBet > getMaxBet board
                                             then
                                               0
                                             else
                                               board^.stepsInRound
                                            )
                          $ board
  where
    activePlayerBet, newCurrentBet :: Int
    activePlayerBet = getFromActivePlayer (^.playerBet) board
    newCurrentBet   = count `max` (getMaxBet board - activePlayerBet)

check :: Board -> Board
check board = bet 0 board

quit :: Board -> Board
quit board = players      %~ Map.delete (board^.activePlayerId)
           $ playersCount %~ pred
           $ board
