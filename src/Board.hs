{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Board where

import Control.Lens   (makeLenses)
import Data.Binary    (Binary)
import Data.Map       (Map)
import Data.Set       (Set)
import GHC.Generics   (Generic)
import Graphics.Gloss (Picture)

import Card (Card, HandValue)

data Player = Player
  { _playerId        :: Int
  , _playerBet       :: Int
  , _playerCards     :: [Card]
  , _playerHandValue :: Maybe HandValue
  , _playerMoney     :: Int
  , _playerName      :: String
  , _isInGame        :: Bool
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
  { _bankParticipants :: Set Int
  , _bankMoney        :: Int
  } deriving (Eq, Generic, Show)

instance Binary Bank where

data PlayerState = Playing
                 | Winner
                 | Loser
  deriving (Eq, Generic, Show)

instance Binary PlayerState where

data Board = Board
  { _onBoardCards        :: [Card]
  , _visibleOnBoardCards :: Hand
  , _players             :: Players
  , _playersCount        :: Int
  , _fixedPlayersCount   :: Int
  , _playerState         :: PlayerState
  , _activePlayerId      :: Int
  , _needAction          :: Bool
  , _needAnyKey          :: Bool
  , _currentBet          :: Int
  , _stepsInRound        :: Int
  , _banks               :: [Bank]
  , _timer               :: Float
} deriving (Eq, Generic, Show)

instance Binary Board where

makeLenses ''Bank
makeLenses ''Board
makeLenses ''Player
