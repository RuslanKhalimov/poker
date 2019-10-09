module CardUtils
  ( fileNameFromCard
  , fileNameFromCardValue
  , handValueFromCardSet
  ) where

import Data.List     (group, groupBy, nub, partition, sort, sortBy)
import Data.Maybe    (fromJust)
import Data.Monoid   (First(..), getFirst)
import Data.Foldable (fold)

import Card (Card (..), CardValue (..), HandValue (..), Rank (..), Suit (..))

fileNameFromCard :: Card -> String
fileNameFromCard CardBack         = "cardBack"
fileNameFromCard (Card cardValue) = fileNameFromCardValue cardValue

fileNameFromCardValue :: CardValue -> String
fileNameFromCardValue cardValue = show (rank cardValue) ++ "Of" ++ show (suit cardValue)

handValueFromCardSet :: [CardValue] -> HandValue
handValueFromCardSet cards = fromJust
                           . getFirst
                           . fold
                           . map (First . ($ cards))
                           $ [ getRoyalFlush
                             , getStraightFlush
                             , getQuads
                             , getFullHouse
                             , getFlush
                             , getStraight
                             , getThreeOfAKind
                             , getTwoPairs
                             , getPair
                             , getKicker
                             ]

getHighCard :: [CardValue] -> Rank
getHighCard = rank . last . sort

groupCardsByRank :: Int -> [CardValue] -> ([[CardValue]], [[CardValue]])
groupCardsByRank n = partition ((>= n) . length) . group . sort

groupCardsBySuit :: [CardValue] -> ([[CardValue]], [[CardValue]])
groupCardsBySuit = partition ((>= 5) . length)
                 . groupBy (\c1 c2 -> suit c1 == suit c2)
                 . sortBy (\c1 c2 -> compare (suit c1) (suit c2))

predRank :: Rank -> Rank
predRank Two = Ace
predRank r   = pred r

checkStraight :: [Rank] -> Bool -- need sorted not empty list
checkStraight [c]            = True
checkStraight (c1 : c2 : cs) = predRank c1 == c2 && checkStraight (c2 : cs)

getStraightRank :: [Rank] -> Maybe Rank -- need sorted not empty list
getStraightRank (c1 : c2 : c3 : c4 : c5 : cs)
  | checkStraight [c1, c2, c3, c4, c5] = Just c1
  | otherwise                              =
    case getStraightRank (c2 : c3 : c4 : c5 : cs) of
      Nothing -> if c1 == Ace
                 then getStraightRank (c2 : (c3 : c4 : c5 : cs) ++ [c1])
                 else Nothing
      rank    -> rank

getStraightRank _ = Nothing

getKicker :: [CardValue] -> Maybe HandValue -- always Just
getKicker = Just . Kicker . getHighCard

getPair :: [CardValue] -> Maybe HandValue
getPair cards =
  if null pairs
  then Nothing
  else Just $ Pair (rank highest) (getHighCard $ concat (other ++ notPair))
    where
      (pairs, notPair) = groupCardsByRank 2 cards
      ([highest, _] : other) = reverse pairs

getTwoPairs :: [CardValue] -> Maybe HandValue
getTwoPairs cards =
  if length pairs < 2
  then Nothing
  else Just $ TwoPairs (rank highest) (rank lower) (getHighCard $ concat (other ++ notPair))
    where
      (pairs, notPair) = groupCardsByRank 2 cards
      ([highest, _] : [lower, _] : other) = reverse pairs

getThreeOfAKind :: [CardValue] -> Maybe HandValue
getThreeOfAKind cards =
  if null triples
  then Nothing
  else Just $ ThreeOfAKind (rank highest) (getHighCard $ concat (other ++ notTriple))
    where
      (triples, notTriple) = groupCardsByRank 3 cards
      ([highest, _, _] : other) = reverse triples

getStraight :: [CardValue] -> Maybe HandValue
getStraight cards = do
  straightRank <- getStraightRank
                . nub
                . reverse
                . sort
                . map rank
                $ cards
  return $ Straight straightRank

getFlush :: [CardValue] -> Maybe HandValue
getFlush cards =
  if null flush
  then Nothing
  else Just $ Flush r1 r2 r3 r4 r5
    where
      (flush, _) = groupCardsBySuit cards
      [r1, r2, r3, r4, r5] = map rank
                           . take 5
                           . reverse
                           . sort
                           . head
                           $ flush

getFullHouse :: [CardValue] -> Maybe HandValue
getFullHouse cards =
  if null triples || null atLeastPairs
  then Nothing
  else Just $ FullHouse (rank highest) (rank . head . last $ atLeastPairs)
    where
      (triples, notTriple) = groupCardsByRank 3 cards
      ([highest, _, _] : other) = reverse triples
      (atLeastPairs, _) = groupCardsByRank 2 (concat $ notTriple ++ other)

getQuads :: [CardValue] -> Maybe HandValue
getQuads cards =
  if null quads
  then Nothing
  else Just $ Quads (rank highest) (getHighCard $ concat (other ++ notQuads))
    where
      (quads, notQuads) = groupCardsByRank 4 cards
      ([highest, _, _, _] : other) = reverse quads

getStraightFlush :: [CardValue] -> Maybe HandValue
getStraightFlush cards =
  if null flush
  then Nothing
  else do
    straightRank <- getStraightRank
                  . nub
                  . map rank
                  . reverse
                  . sort
                  . head
                  $ flush
    return $ StraightFlush straightRank
      where
        (flush, _) = groupCardsBySuit cards

getRoyalFlush :: [CardValue] -> Maybe HandValue
getRoyalFlush cards = do
  handValue <- getStraightFlush cards
  if handValue == StraightFlush Ace
  then
    return RoyalFlush
  else
    Nothing
