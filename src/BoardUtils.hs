module BoardUtils
  ( addBet
  , createBoard
  , createBoardUsingGen
  , fillBanks
  , getFromActivePlayer
  , getHandValues
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

import           Control.Lens          ((^.), (.~), (%~))
import           Data.List             (zip4)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           System.Random         (StdGen, mkStdGen)
import           System.Random.Shuffle (shuffle')
import           System.Time           (ClockTime (TOD), getClockTime)

import Board
import Card        (Card (..), CardValue, HandValue)
import CardUtils   (handValueFromCardSet)

bigBlind, smallBlind, initialMoney :: Int
smallBlind   = 25
bigBlind     = 2 * smallBlind
initialMoney = 1000

hideCards :: Hand -> Int -> Board -> Board
hideCards hand exceptId board = onBoardCards .~ newOnBoardCards
                              $ players      .~ newPlayers
                              $ needAction   .~ (exceptId == board^.activePlayerId)
                              $ board
  where
    newOnBoardCards = take (fromEnum hand) (board^.onBoardCards) ++ replicate (5 - fromEnum hand) CardBack
    newPlayers      = Map.map hidePlayer'sCards $ board^.players

    hidePlayer'sCards :: Player -> Player
    hidePlayer'sCards player
      | exceptId == player^.playerId = player
      | otherwise                    = playerCards .~ [CardBack, CardBack] $ player

createBoard :: Int -> IO Board
createBoard cnt = do
  (TOD time _) <- getClockTime
  return $ createBoardUsingGen time 0 cnt

nextDeal :: Integer -> Int -> Bool -> Board -> Board
nextDeal time smallBlindId isFirstTime board = setBlinds smallBlindId
                                             $ onBoardCards        .~ map Card onBoard
                                             $ visibleOnBoardCards .~ PreFlop
                                             $ players             .~ createdPlayers
                                             $ playersCount        .~ Map.size createdPlayers
                                             $ currentBet          .~ 0
                                             $ stepsInRound        .~ 0
                                             $ banks               .~ [Bank (Map.keysSet createdPlayers) 0]
                                             $ board
  where
    cnt                     = board^.fixedPlayersCount
    gen                     = mkStdGen $ fromInteger time
    (money, names)          = if isFirstTime
                              then
                                ( Map.fromList . zip [0..] $ replicate cnt initialMoney
                                , Map.fromList . zip [0..] $ replicate cnt ""
                                )
                              else
                                ( Map.map (^.playerMoney) $ board^.players
                                , Map.map (^.playerName)  $ board^.players
                                )
    deck                    = map toEnum $ shuffle' [0..51] 52 gen
    (onBoard, playersCards) = dealCards cnt deck []
    createdPlayers          = Map.filterWithKey (\_id _ -> isFirstTime || Map.member _id (board^.players))
                            $ createPlayers (map (map Card) playersCards) money names

    dealCards :: Int -> [CardValue] -> [[CardValue]] -> ([CardValue], [[CardValue]])
    dealCards 0   deck acc = (take 5 deck, acc)
    dealCards cnt deck acc = dealCards (cnt - 1) (drop 2 deck) (take 2 deck : acc)

addBet :: Int -> Player -> Player
addBet x player = playerBet   %~ (+x)
                $ playerMoney %~ (x `subtract`)
                $ player

setBlinds :: Int -> Board -> Board
setBlinds smallBlindId board = activePlayerId .~ getNextId board (getNextId board smallBlindId)
                             $ players        %~ updatePlayers
                             $ board
  where
    updatePlayers :: Players -> Players
    updatePlayers = Map.adjust (addBet smallBlind) smallBlindId
                  . Map.adjust (addBet bigBlind) (getNextId board smallBlindId)

createBoardUsingGen :: Integer -> Int -> Int -> Board
createBoardUsingGen time smallBlindId cnt = nextDeal time smallBlindId True
                                          $ Board { _onBoardCards        = []
                                                  , _visibleOnBoardCards = PreFlop
                                                  , _playersCount        = cnt
                                                  , _fixedPlayersCount   = cnt
                                                  , _playerState         = Playing
                                                  , _activePlayerId      = -1
                                                  , _needAction          = True
                                                  , _needAnyKey          = False
                                                  , _currentBet          = 0
                                                  , _stepsInRound        = 0
                                                  , _banks               = [Bank (Set.fromList [0..cnt - 1]) 0]
                                                  , _timer               = 30
                                                  , _players             = Map.empty
                                                  }

createPlayers :: [[Card]] -> Map.Map Int Int -> Map.Map Int String -> Players
createPlayers cards money names = Map.fromList
                                . map createPlayerEntry
                                $ zip4 (Set.toList $ Map.keysSet names) cards (Map.elems money) (Map.elems names)

createPlayerEntry :: (Int, [Card], Int, String) -> (Int, Player)
createPlayerEntry (_id, cards, money, name) = (_id, player)
  where
    player = Player { _playerId        = _id
                    , _playerBet       = 0
                    , _playerCards     = cards
                    , _playerMoney     = money
                    , _playerName      = name
                    , _isInGame        = True
                    , _playerHandValue = Nothing
                    }

getFromActivePlayer :: (Player -> a) -> Board -> a
getFromActivePlayer getter board = getter $ (board^.players) Map.! (board^.activePlayerId)

modifyPlayer :: Int -> (Player -> Player) -> Board -> Board
modifyPlayer _id func = players %~ Map.adjust func _id

modifyActivePlayer :: (Player -> Player) -> Board -> Board
modifyActivePlayer func board = modifyPlayer (board^.activePlayerId) func board

isRoundFinished :: Board -> Bool
isRoundFinished board = board^.visibleOnBoardCards == River
                     && Map.size (Map.filter (^.isInGame) $ board^.players) == board^.stepsInRound
                     || (Map.size $ Map.filter (^.isInGame) $ board^.players) == 1
                     || (Map.size $ Map.filter ((> 0) . (^.playerMoney)) $ board^.players) < 2
                     && all (\p -> p^.playerBet == getMaxBet board || p^.playerMoney == 0) (board^.players)

isGameFinished :: Board -> Bool
isGameFinished board = 1 == board^.playersCount

kickPlayers :: Board -> Board
kickPlayers board = playersCount .~ Map.size newPlayers
                  $ players      .~ newPlayers
                  $ board
  where
    newPlayers = Map.filter ((> 0) . (^.playerMoney)) $ board^.players

getHandValues :: Board -> Map.Map Int HandValue
getHandValues board = Map.map handValueFromCardSet
                    . Map.map (map (\(Card cv) -> cv) . ((board^.onBoardCards) ++) . (^.playerCards))
                    . Map.filter (^.isInGame)
                    $ board^.players

giveMoney :: Board -> Board
giveMoney board = _giveMoney (filterParticipants (getHandValues board) $ board^.banks) board
  where
    initialBank :: Bank
    initialBank = Bank { _bankMoney        = 0
                       , _bankParticipants = Map.keysSet $ board^.players
                       }

    filterParticipants :: Map.Map Int HandValue -> [Bank] -> [Bank]
    filterParticipants handValues []        = []
    filterParticipants handValues (bank:bs) =
      let participants   = Map.filterWithKey (\k _ -> Set.member k $ bank^.bankParticipants) handValues
          winCombination = maximum participants
          winPlayers     = Map.filter (== winCombination) participants
          filteredBank   = bankParticipants %~ Set.filter (flip Map.member winPlayers) $ bank
      in
        (filteredBank : filterParticipants handValues bs)

    _giveMoney :: [Bank] -> Board -> Board
    _giveMoney []        board = board
    _giveMoney (bank:bs) board = banks .~ (bank:bs)
                               $ _giveMoney bs
                               $ players %~ Map.map (giveMoneyToPlayer bank)
                               $ board

    giveMoneyToPlayer :: Bank -> Player -> Player
    giveMoneyToPlayer bank player =
      let handValues      = getHandValues board
          participants    = Map.filterWithKey (\k _ -> Set.member k $ bank^.bankParticipants) handValues
          winCombination  = maximum participants
          winPlayers      = Map.filter (== winCombination) participants
      in
        if Map.member (player^.playerId) winPlayers
        then
          playerMoney %~ (+ (bank^.bankMoney `div` Map.size winPlayers)) $ player
        else
          player

getMaxBet :: Board -> Int
getMaxBet = maximum . Map.map (^.playerBet) . (^.players)

getNextId :: Board -> Int -> Int
getNextId board _id = _getNextId $ nextPlayerId _id
  where
    _getNextId :: Int -> Int
    _getNextId _id =
      if board^.visibleOnBoardCards == Showdown
         || Map.member _id (board^.players)
         && ((board^.players) Map.! _id)^.isInGame
         && ((board^.players) Map.! _id)^.playerMoney > 0
      then
        _id
      else
        _getNextId $ nextPlayerId _id

    playersCnt :: Int
    playersCnt = board^.fixedPlayersCount

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
    bank:bs       = board^.banks
    playerBets    = Map.map (^.playerBet) $ board^.players
    nonZeroBets   = Map.filter (> 0) . Map.map (^.playerBet) . Map.filter (^.isInGame) $ board^.players
    minBet        = if Map.null nonZeroBets then 0 else minimum nonZeroBets
    forEqualBank  = bankMoney %~ (+ (sum . Map.map (^.playerBet) $ board^.players)) $ bank
    newBank       = bankMoney %~ (+ (sum . Map.map (min minBet . (^.playerBet)) $ board^.players)) $ bank
    newPlayerBets = Map.map (max 0 . (minBet `subtract`)) playerBets
    newPlayers    = Map.map (\p -> playerBet .~ newPlayerBets Map.! (p^.playerId) $ p) $ board^.players
  in
    if Map.null nonZeroBets || all (== minBet) nonZeroBets
    then banks   .~ forEqualBank:bs
       $ players .~ newPlayers
       $ board
    else
      fillBanks $ banks   .~ (Bank { _bankMoney        = 0
                                   , _bankParticipants = Map.keysSet $ Map.filter ((> 0) . (^.playerBet)) newPlayers
                                   }
                             ) : newBank : bs
                $ players .~ newPlayers
                $ board

mergeBoards :: Board -> Board -> Board
mergeBoards board oldBoard = stepsInRound .~ board^.stepsInRound
                           $ players      .~ newPlayers
                           $ playersCount .~ Map.size newPlayers
                           $ oldBoard
  where
    _id :: Int
    _id = board^.activePlayerId

    newPlayers :: Players
    newPlayers = if Map.member _id $ board^.players
                 then
                   Map.insert _id ((board^.players) Map.! _id) $ oldBoard^.players
                 else
                   Map.delete _id $ oldBoard^.players
