{-# LANGUAGE DeriveGeneric #-}

module Board
  ( Bank (..)
  , Board (..)
  , Hand (..)
  , Player (..)
  , Players
  ) where

import Data.Binary    (Binary)
import Data.Map       (Map)
import Data.Set       (Set)
import GHC.Generics   (Generic)
import Graphics.Gloss (Picture)

import Card (Card, HandValue)

data Player = Player
  { playerId        :: Int
  , playerBet       :: Int
  , playerCards     :: [Card]
  , playerHandValue :: Maybe HandValue
  , playerMoney     :: Int
  , playerName      :: String
  , isInGame        :: Bool
  } deriving (Eq, Generic, Show)

instance Binary Player where

type Players = Map Int Player

data Hand = PreFlop
          | Flop
          | Turn
          | River
          | Showdown
  deriving (Eq, Generic, Show)

instance Binary Hand where

instance Enum Hand where
  fromEnum PreFlop  = 0
  fromEnum Flop     = 3
  fromEnum Turn     = 4
  fromEnum River    = 5
  fromEnum Showdown = 5

  toEnum = undefined

  succ PreFlop  = Flop
  succ Flop     = Turn
  succ Turn     = River
  succ River    = Showdown
  succ Showdown = Showdown

data Bank = Bank
  { participants :: Set Int
  , money        :: Int
  } deriving (Eq, Generic, Show)

instance Binary Bank where

data Board = Board
  { onBoardCards        :: [Card]
  , visibleOnBoardCards :: Hand
  , players             :: Players
  , playersCount        :: Int
  , fixedPlayersCount   :: Int
  , activePlayerId      :: Int
  , needAction          :: Bool
  , needAnyKey          :: Bool
  , currentBet          :: Int
  , stepsInRound        :: Int
  , banks               :: [Bank]
  , timer               :: Float
} deriving (Eq, Generic, Show)

instance Binary Board where
