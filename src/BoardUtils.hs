module BoardUtils
  ( createBoard
  , createBoardUsingGen
  , fillBanks
  , getCardSets
  , getFromActivePlayer
  , getMaxBet
  , giveMoney
  , getNextId
  , getNextPlayerId
  , hideCards
  , isGameFinished
  , isRoundFinished
  , kickPlayers
  , mergeBoards
  , modifyActivePlayer
  , modifyPlayer
  , nextDeal
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Random           (StdGen, mkStdGen)
import System.Random.Shuffle   (shuffle')
import System.Time             (ClockTime (TOD), getClockTime)

import Board       (Bank (..), Board (..), Hand (..), Player (..), Players)
import Card        (Card (..), CardValue, HandValue)

bigBlind, smallBlind, initialMoney :: Int
smallBlind   = 25
bigBlind     = 2 * smallBlind
initialMoney = 1000

hideCards :: Hand -> Int -> Board -> Board
hideCards hand exceptId board =
  let newOnBoardCards = take (fromEnum hand) (onBoardCards board) ++ replicate (5 - fromEnum hand) CardBack
      newPlayers      = Map.map (hidePlayer'sCards exceptId) $ players board
      newBoard        = board { onBoardCards = newOnBoardCards
                              , players      = newPlayers
                              , needAction   = exceptId == activePlayerId board
                              }
  in newBoard

hidePlayer'sCards :: Int -> Player -> Player
hidePlayer'sCards exceptId player
  | exceptId == playerId player = player
  | otherwise                   = player { playerCards = [CardBack, CardBack] }

dealCards :: Int -> [CardValue] -> [[CardValue]] -> ([CardValue], [[CardValue]])
dealCards 0   deck acc = (take 5 deck, acc)
dealCards cnt deck acc = dealCards (cnt - 1) (drop 2 deck) (take 2 deck : acc)

createBoard :: Int -> Int -> IO Board
createBoard smallBlindId cnt = do
  (TOD sec _) <- getClockTime
  let gen = mkStdGen $ fromInteger sec
  return $ createBoardUsingGen gen smallBlindId cnt

nextDeal :: StdGen -> Int -> Int -> Bool -> Board -> Board
nextDeal gen smallBlindId cnt isFirstTime board =
  let money                   = if isFirstTime
                                then
                                  replicate cnt initialMoney
                                else
                                  map snd . Map.toList . (Map.map playerMoney) . players $ board
      deck                    = map toEnum $ shuffle' [0..51] 52 gen
      (onBoard, playersCards) = dealCards cnt deck []
      createdPlayers          = createPlayers smallBlindId cnt (map (map Card) playersCards) money
  in
    board { onBoardCards = map Card onBoard
          , players      = createdPlayers
          }

createBoardUsingGen :: StdGen -> Int -> Int -> Board
createBoardUsingGen gen smallBlindId cnt =
  nextDeal gen smallBlindId cnt True $ Board { onBoardCards        = []
                                             , visibleOnBoardCards = PreFlop
                                             , playersCount        = cnt
                                             , activePlayerId      = nextPlayerId cnt . nextPlayerId cnt $ smallBlindId
                                             , needAction          = True
                                             , needAnyKey          = False
                                             , currentBet          = 0
                                             , stepsInRound        = 0
                                             , banks               = [Bank (Set.fromList [0..cnt]) 0]
                                             , timer               = 30
                                             , players             = Map.empty
                                             }

createPlayers :: Int -> Int -> [[Card]] -> [Int] -> Players
createPlayers smallBlindId cnt cards money  = Map.fromList
                                            . map (createPlayerEntry smallBlindId cnt)
                                            $ zip3 [0..(cnt - 1)] cards money

createPlayerEntry :: Int -> Int -> (Int, [Card], Int) -> (Int, Player)
createPlayerEntry smallBlindId cnt (_id, cards, money) =
  let
    initialBet = if smallBlindId == _id
                 then smallBlind
                 else if nextPlayerId cnt smallBlindId == _id
                 then bigBlind
                 else 0
    player = Player { playerId    = _id
                    , playerBet   = initialBet
                    , playerCards = cards
                    , playerMoney = money - initialBet
                    , isInGame    = True
                    }
  in (_id, player)

getFromActivePlayer :: (Player -> a) -> Board -> a
getFromActivePlayer getter board = getter $ players board Map.! (activePlayerId board)

modifyPlayer :: Int -> (Player -> Player) -> Board -> Board
modifyPlayer _id func board = board { players = Map.adjust func _id (players board) }

modifyActivePlayer :: (Player -> Player) -> Board -> Board
modifyActivePlayer func board = modifyPlayer (activePlayerId board) func board

isRoundFinished :: Board -> Bool
isRoundFinished board = visibleOnBoardCards board == Showdown
                     || (Map.size $ Map.filter isInGame (players board)) == 1
                     || (Map.size $ Map.filter ((> 0) . playerMoney) (players board)) < 2

isGameFinished :: Board -> Bool
isGameFinished board = 1 == (Map.size $ Map.filter ((> 0) . playerMoney) (players board))

kickPlayers :: Board -> Board
kickPlayers board = board { playersCount = Map.size newPlayers
                          , players      = newPlayers
                          }
  where
    newPlayers = Map.filter ((> 0) . playerMoney) (players board)

giveMoney :: Map.Map Int HandValue -> [Bank] -> Board -> Board
giveMoney _          []        board = board
giveMoney handValues (bank:bs) board = giveMoney handValues bs board { players = Map.map (giveMoneyToPlayer bank) (players board) }
  where
    giveMoneyToPlayer :: Bank -> Player -> Player
    giveMoneyToPlayer bank player =
      let _participants   = Map.filterWithKey (\k _ -> Set.member k (participants bank)) handValues
          winCombination  = maximum _participants
          winPlayers      = Map.filter (== winCombination) _participants
      in
        if Map.member (playerId player) winPlayers
        then
          player { playerMoney = playerMoney player + money bank `div` Map.size winPlayers }
        else
          player

getCardSets :: Board -> Map.Map Int [CardValue]
getCardSets board = Map.map (map (\(Card cv) -> cv) . (onBoardCards board ++) . playerCards)
                  . Map.filter isInGame
                  $ (players board)

getMaxBet :: Board -> Int
getMaxBet = maximum . Map.map playerBet . players

getNextPlayerId :: Board -> Int
getNextPlayerId board = getNextId (nextPlayerId (playersCount board) (activePlayerId board)) board

getNextId :: Int -> Board -> Int
getNextId _id board =
  if visibleOnBoardCards board == Showdown
     || isInGame (players board Map.! _id) && playerMoney (players board Map.! _id) > 0
  then
    _id
  else
    getNextId (nextPlayerId (playersCount board) _id) board

nextPlayerId :: Int -> Int -> Int
nextPlayerId playersCnt _id = if playersCnt == 4
                              then case _id of
                                0 -> 3
                                1 -> 2
                                2 -> 0
                                3 -> 1
                              else
                                ((_id + 1) `mod` playersCnt)
fillBanks :: Board -> Board
fillBanks board =
  let
    bank:bs       = banks board
    playerBets    = Map.filter (> 0) . Map.map playerBet $ players board
    minBet        = minimum playerBets
    newBank       = bank { money = money bank + (sum . Map.map playerBet . players $ board) }
    newPlayerBets = Map.map (+ (-minBet)) playerBets
  in
    if all (== minBet) playerBets
    then
      board { banks   = newBank:bs
            , players = Map.map (\p -> p { playerBet = newPlayerBets Map.! playerId p } ) (players board)
            }
    else
      let newPlayers = Map.map (\p -> p { playerBet = newPlayerBets Map.! playerId p } ) (players board) in
      board { banks  = (Bank { money = 0
                              , participants = Map.keysSet $ Map.filter ((> 0) . playerBet) newPlayers
                              }
                         ) : newBank : bs
            , players = newPlayers
            }

mergeBoards :: Board -> Board -> Board
mergeBoards board oldBoard = oldBoard { stepsInRound = stepsInRound board
                                      , players      = Map.insert _id (players board Map.! _id) (players oldBoard)
                                      }
  where
    _id = activePlayerId board
