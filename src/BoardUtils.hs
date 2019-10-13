module BoardUtils
  ( addBet
  , createBoard
  , createBoardUsingGen
  , fillBanks
  , getFromActivePlayer
  , getMaxBet
  , giveMoney
  , getNextId
  , hideCards
  , isGameFinished
  , isRoundFinished
  , kickPlayers
  , mergeBoards
  , modifyActivePlayer
  , modifyPlayer
  , nextDeal
  ) where

import           Data.List             (zip4)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           System.Random         (StdGen, mkStdGen)
import           System.Random.Shuffle (shuffle')
import           System.Time           (ClockTime (TOD), getClockTime)

import Board       (Bank (..), Board (..), Hand (..), Player (..), Players)
import Card        (Card (..), CardValue, HandValue)
import CardUtils   (handValueFromCardSet)

bigBlind, smallBlind, initialMoney :: Int
smallBlind   = 25
bigBlind     = 2 * smallBlind
initialMoney = 1000

hideCards :: Hand -> Int -> Board -> Board
hideCards hand exceptId board = board { onBoardCards = newOnBoardCards
                                      , players      = newPlayers
                                      , needAction   = exceptId == activePlayerId board
                                      }
  where
    newOnBoardCards = take (fromEnum hand) (onBoardCards board) ++ replicate (5 - fromEnum hand) CardBack
    newPlayers      = Map.map hidePlayer'sCards $ players board

    hidePlayer'sCards :: Player -> Player
    hidePlayer'sCards player
      | exceptId == playerId player = player
      | otherwise                   = player { playerCards = [CardBack, CardBack] }

createBoard :: Int -> IO Board
createBoard cnt = do
  (TOD time _) <- getClockTime
  return $ createBoardUsingGen time 0 cnt

nextDeal :: Integer -> Int -> Bool -> Board -> Board
nextDeal time smallBlindId isFirstTime board = setBlinds smallBlindId
                                             $ board { onBoardCards        = map Card onBoard
                                                     , visibleOnBoardCards = PreFlop
                                                     , players             = createdPlayers
                                                     , playersCount        = Map.size createdPlayers
                                                     , currentBet          = 0
                                                     , stepsInRound        = 0
                                                     , banks               = [Bank (Map.keysSet createdPlayers) 0]
                                                     }
  where
    cnt                     = fixedPlayersCount board
    gen                     = mkStdGen $ fromInteger time
    (money, names)          = if isFirstTime
                              then
                                ( Map.fromList . zip [0..] $ replicate cnt initialMoney
                                , Map.fromList . zip [0..] $ replicate cnt ""
                                )
                              else
                                ( Map.map playerMoney $ players board
                                , Map.map playerName  $ players board
                                )
    deck                    = map toEnum $ shuffle' [0..51] 52 gen
    (onBoard, playersCards) = dealCards cnt deck []
    createdPlayers          = Map.filterWithKey (\_id _ -> isFirstTime || Map.member _id (players board))
                            $ createPlayers (map (map Card) playersCards) money names

    dealCards :: Int -> [CardValue] -> [[CardValue]] -> ([CardValue], [[CardValue]])
    dealCards 0   deck acc = (take 5 deck, acc)
    dealCards cnt deck acc = dealCards (cnt - 1) (drop 2 deck) (take 2 deck : acc)

addBet :: Int -> Player -> Player
addBet x player = player { playerBet   = playerBet player + x
                         , playerMoney = playerMoney player - x
                         }

setBlinds :: Int -> Board -> Board
setBlinds smallBlindId board = board { activePlayerId = getNextId board bigBlindId
                                     , players        = updatePlayers $ players board
                                     }
  where
    bigBlindId :: Int
    bigBlindId = getNextId board smallBlindId

    updatePlayers :: Players -> Players
    updatePlayers = Map.adjust (addBet smallBlind) smallBlindId
                  . Map.adjust (addBet bigBlind)   bigBlindId

createBoardUsingGen :: Integer -> Int -> Int -> Board
createBoardUsingGen time smallBlindId cnt = nextDeal time smallBlindId True
                                          $ Board { onBoardCards        = []
                                                  , visibleOnBoardCards = PreFlop
                                                  , playersCount        = cnt
                                                  , fixedPlayersCount   = cnt
                                                  , activePlayerId      = -1
                                                  , needAction          = True
                                                  , needAnyKey          = False
                                                  , currentBet          = 0
                                                  , stepsInRound        = 0
                                                  , banks               = [Bank (Set.fromList [0..cnt - 1]) 0]
                                                  , timer               = 30
                                                  , players             = Map.empty
                                                  }

createPlayers :: [[Card]] -> Map.Map Int Int -> Map.Map Int String -> Players
createPlayers cards money names = Map.fromList
                                . map createPlayerEntry
                                $ zip4 (Set.toList $ Map.keysSet names) cards (Map.elems money) (Map.elems names)

createPlayerEntry :: (Int, [Card], Int, String) -> (Int, Player)
createPlayerEntry (_id, cards, money, name) = (_id, player)
  where
    player = Player { playerId    = _id
                    , playerBet   = 0
                    , playerCards = cards
                    , playerMoney = money
                    , playerName  = name
                    , isInGame    = True
                    }

getFromActivePlayer :: (Player -> a) -> Board -> a
getFromActivePlayer getter board = getter $ players board Map.! (activePlayerId board)

modifyPlayer :: Int -> (Player -> Player) -> Board -> Board
modifyPlayer _id func board = board { players = Map.adjust func _id (players board) }

modifyActivePlayer :: (Player -> Player) -> Board -> Board
modifyActivePlayer func board = modifyPlayer (activePlayerId board) func board

isRoundFinished :: Board -> Bool
isRoundFinished board = visibleOnBoardCards board == River
                     && Map.size (Map.filter isInGame $ players board) == stepsInRound board
                     || (Map.size $ Map.filter isInGame (players board)) == 1
                     || (Map.size $ Map.filter ((> 0) . playerMoney) (players board)) < 2
                     && all (\p -> playerBet p == getMaxBet board || playerMoney p == 0) (players board)

isGameFinished :: Board -> Bool
isGameFinished board = 1 == playersCount board

kickPlayers :: Board -> Board
kickPlayers board = board { playersCount = Map.size newPlayers
                          , players      = newPlayers
                          }
  where
    newPlayers = Map.filter ((> 0) . playerMoney) (players board)

giveMoney :: Board -> Board
giveMoney board = _giveMoney (banks board) board
  where
    cardSets :: Map.Map Int [CardValue]
    cardSets = Map.map (map (\(Card cv) -> cv) . (onBoardCards board ++) . playerCards)
             . Map.filter isInGame
             $ (players board)

    handValues :: Map.Map Int HandValue
    handValues = Map.map handValueFromCardSet cardSets

    _giveMoney :: [Bank] -> Board -> Board
    _giveMoney []        board = board
    _giveMoney (bank:bs) board = _giveMoney bs
                               $ board { players = Map.map (giveMoneyToPlayer bank) (players board) }

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

getMaxBet :: Board -> Int
getMaxBet = maximum . Map.map playerBet . players

getNextId :: Board -> Int -> Int
getNextId board _id = _getNextId $ nextPlayerId _id
  where
    _getNextId :: Int -> Int
    _getNextId _id =
      if visibleOnBoardCards board == Showdown
         || Map.member _id (players board)
         && isInGame (players board Map.! _id)
         && playerMoney (players board Map.! _id) > 0
      then
        _id
      else
        _getNextId $ nextPlayerId _id

    playersCnt :: Int
    playersCnt = fixedPlayersCount board

    nextPlayerId :: Int -> Int
    nextPlayerId _id = if playersCnt == 4
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
    playerBets    = Map.map playerBet . Map.filter isInGame . players $ board
    minBet        = minimum playerBets
    newBank       = bank { money = money bank + (sum . Map.map playerBet . players $ board) }
    newPlayerBets = Map.map (`subtract` minBet) playerBets
    newPlayers    = Map.map (\p -> p { playerBet = newPlayerBets Map.! playerId p } ) (players board)
  in
    if all (== minBet) playerBets
    then
      board { banks   = newBank:bs
            , players = newPlayers
            }
    else
      board { banks   = (Bank { money = sum . Map.map playerBet . players $ board
                              , participants = Map.keysSet $ Map.filter ((> 0) . playerBet) newPlayers
                              }
                        ) : bank : bs
            , players = newPlayers
            }

mergeBoards :: Board -> Board -> Board
mergeBoards board oldBoard = oldBoard { stepsInRound = stepsInRound board
                                      , players      = newPlayers
                                      , playersCount = Map.size newPlayers
                                      }
  where
    _id :: Int
    _id = activePlayerId board

    newPlayers :: Players
    newPlayers = if Map.member _id (players board)
                 then
                   Map.insert _id (players board Map.! _id) (players oldBoard)
                 else
                   Map.delete _id (players oldBoard)
