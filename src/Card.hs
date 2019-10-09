{-# LANGUAGE DeriveGeneric #-}

module Card
  ( Card (..)
  , CardValue (..)
  , HandValue (..)
  , Rank (..)
  , Suit (..)
  ) where

import Data.Binary  (Binary)
import GHC.Generics (Generic)

data Rank = Two
          | Three
          | Four
          | Five
          | Six
          | Seven
          | Eight
          | Nine
          | Ten
          | Jack
          | Queen
          | King
          | Ace
  deriving (Enum, Eq, Generic, Ord, Show)

instance Binary Rank where

data Suit = Clubs
          | Diamonds
          | Hearts
          | Spades
  deriving (Enum, Eq, Generic, Ord, Show)

instance Binary Suit where

data CardValue = CardValue { rank :: Rank, suit :: Suit }
  deriving (Generic, Show)

instance Binary CardValue where

instance Eq CardValue where
  a == b = rank a == rank b

instance Ord CardValue where
  a <= b = rank a <= rank b

instance Enum CardValue where
  fromEnum card = 4 * (fromEnum . rank) card + (fromEnum . suit) card
  toEnum x      = CardValue (toEnum $ x `div` 4) (toEnum $ x `mod` 4)

data Card = CardBack | Card CardValue deriving (Eq, Generic, Show)

instance Binary Card where

data HandValue = Kicker Rank
               | Pair Rank Rank
               | TwoPairs Rank Rank Rank
               | ThreeOfAKind Rank Rank
               | Straight Rank
               | Flush Rank Rank Rank Rank Rank
               | FullHouse Rank Rank
               | Quads Rank Rank
               | StraightFlush Rank
               | RoyalFlush
  deriving (Eq, Generic, Ord, Show)

instance Binary HandValue where
