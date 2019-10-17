{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main
  ) where

import           Control.Concurrent             (forkFinally)
import           Control.Concurrent.MVar        (MVar, newEmptyMVar, putMVar, takeMVar)
import           Control.Exception              (IOException, SomeException, bracket, try)
import           Control.Lens                   ((^.), (.~), (%~))
import           Control.Monad                  (replicateM, when)
import           Data.Binary                    (encode, decode)
import qualified Data.Map as Map
import           Network.Socket          hiding (recv, sendAll)
import           Network.Socket.ByteString.Lazy (recv, sendAll)
import           System.Environment             (getArgs)
import           System.Exit                    (exitFailure)
import           System.Time                    (ClockTime(TOD), getClockTime)

import           Board        ( Board, Hand(Showdown), PlayerState(..), activePlayerId, banks, isInGame, needAction
                              , needAnyKey, playerBet, playerHandValue, playerId, playerName, players, playerState
                              , playersCount, stepsInRound, visibleOnBoardCards)
import qualified BoardUtils as BU
import           PlayerAction (PlayerAction (..), bet, check, foldCards, quit)

type Connections      = Map.Map Int Socket
type SharedPlayerInfo = MVar (String, Socket)

main :: IO()
main = withSocketsDo $ do
  args <- getArgs
  port <- case args of [port] -> return port
                       _      -> do
                                   putStrLn "Incorrect arguments"
                                   putStrLn "Excpected <port>"
                                   exitFailure

  addr <- resolve port
  bracket (open addr) close runServer

resolve :: String -> IO AddrInfo
resolve port = do
  let hints = defaultHints { addrFlags = [AI_PASSIVE]
                           , addrSocketType = Stream
                           }
  addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
  return addr

open :: AddrInfo -> IO Socket
open addr = do
  serverSocket <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  setSocketOption serverSocket ReuseAddr 1
  bind serverSocket (addrAddress addr)
  listen serverSocket 10
  return serverSocket

runGame :: Int -> SharedPlayerInfo -> IO ()
runGame count mVar = do
  initialBoard        <- BU.createBoard count
  playersInfo         <- replicateM count $ takeMVar mVar
  let (names, sockets) = unzip playersInfo
  let boardWithNames   = players %~ Map.map (\p -> playerName .~ names !! (p^.playerId) $ p) $ initialBoard
  let connections      = Map.fromList $ zip [0..] sockets
  putStrLn "Start game with players : "
  mapM_ printPlayerInfo playersInfo
  forkFinally (gameLoop 0 connections boardWithNames) (handleException playersInfo)
  runGame count mVar
    where
      printPlayerInfo :: (String, Socket) -> IO ()
      printPlayerInfo (name, socket) =
        putStrLn $ "  " ++ show name ++ " from " ++ show socket

      handleException :: [(String, Socket)] -> Either SomeException a -> IO ()
      handleException playersInfo (Left exception) = do
        putStrLn $ "Exception (" ++ show exception ++ ") in game with players"
        mapM_ printPlayerInfo playersInfo
      handleException playersInfo _                = do
        putStrLn "Game finished,  players : "
        mapM_ printPlayerInfo playersInfo

runServer :: Socket -> IO ()
runServer serverSocket = do
  twoPlayersMVar   <- newEmptyMVar
  threePlayersMVar <- newEmptyMVar
  fourPlayersMVar  <- newEmptyMVar
  forkFinally (runGame 2 twoPlayersMVar)   (handleException 2)
  forkFinally (runGame 3 threePlayersMVar) (handleException 3)
  forkFinally (runGame 4 fourPlayersMVar)  (handleException 4)
  acceptPlayersLoop twoPlayersMVar threePlayersMVar fourPlayersMVar serverSocket
    where
      handleException :: Int -> Either SomeException a -> IO ()
      handleException count (Left exception) =
        putStrLn $ "Exception (" ++ show exception ++ ") in server with " ++ show count ++ " players"
      handleException count _                =
        putStrLn $ "Server with " ++ show count ++ " players finished"

acceptPlayersLoop :: SharedPlayerInfo -> SharedPlayerInfo -> SharedPlayerInfo -> Socket -> IO ()
acceptPlayersLoop twoPlayersMVar threePlayersMVar fourPlayersMVar serverSocket = do
  (name, count, clientSocket) <- waitPlayerConnection serverSocket
  case count of
    2 -> successfulConnection name clientSocket twoPlayersMVar
    3 -> successfulConnection name clientSocket threePlayersMVar
    4 -> successfulConnection name clientSocket fourPlayersMVar
    _ -> sendAll clientSocket $ encode "Unsupported players count"
  acceptPlayersLoop twoPlayersMVar threePlayersMVar fourPlayersMVar serverSocket

successfulConnection :: String -> Socket -> SharedPlayerInfo -> IO ()
successfulConnection name clientSocket mVar = do
  sendAll clientSocket $ encode "Connected"
  putMVar mVar (name, clientSocket)

waitPlayerConnection :: Socket -> IO (String, Int, Socket)
waitPlayerConnection serverSocket = do
  (clientSocket, addr) <- accept serverSocket
  name               <- fmap decode $ recv clientSocket 32
  putStrLn $ name ++ " from " ++ show addr ++ " connected"
  count              <- fmap decode $ recv clientSocket 32
  return (name, count, clientSocket)

applyAction :: PlayerAction -> Board -> Board
applyAction (Bet x) = bet x
applyAction Check   = check
applyAction Fold    = foldCards
applyAction Ok      = id
applyAction Quit    = quit

gameLoop :: Int -> Connections -> Board -> IO ()
gameLoop firstPlayerId connections board = do
  shareBoard connections board
  action            <- recvAction (connections Map.! (board^.activePlayerId))
  let updatedBoard   = applyAction action board
  let newConnections = Map.filterWithKey (\_id _ -> Map.member _id $ updatedBoard^.players) connections

  let newBoard = BU.mergeBoards updatedBoard board
  if BU.isRoundFinished newBoard
  then do
    finalBoard <- finishRound firstPlayerId newConnections newBoard
    if BU.isGameFinished finalBoard
    then
      finishGame newConnections finalBoard
    else do
      (TOD time _) <- getClockTime
      let nextRoundBoard = BU.nextDeal time (BU.getNextId finalBoard firstPlayerId) False finalBoard
      let nextPlayerId  = BU.getNextId nextRoundBoard firstPlayerId
      gameLoop nextPlayerId newConnections nextRoundBoard
  else
    if newBoard^.stepsInRound == Map.size (Map.filter (^.isInGame) $ newBoard^.players)
    then do
      let finalBoard = visibleOnBoardCards %~ succ
                     $ stepsInRound        .~ 0
                     $ banks               .~ (BU.fillBanks newBoard)^.banks
                     $ activePlayerId      .~ (BU.getNextId newBoard . BU.getNextId newBoard $ firstPlayerId)
                     $ players             %~ Map.map (playerBet .~ 0)
                     $ newBoard
      gameLoop firstPlayerId newConnections finalBoard
    else do
      gameLoop firstPlayerId newConnections $ activePlayerId %~ BU.getNextId newBoard $ newBoard

sendBoard :: Board -> (Int, Socket) -> IO ()
sendBoard board (_id, socket) = do
  let updatedPlayerState = if Map.member _id $ board^.players
                           then if board^.playersCount == 1
                                then
                                  Winner
                                else
                                  Playing
                           else
                             Loser
  (_ :: Either IOException ()) <- try
    $ sendAll socket . encode
    $ playerState .~ updatedPlayerState
    $ (if board^.visibleOnBoardCards == Showdown
       then (needAction .~ False)
       else BU.hideCards (board^.visibleOnBoardCards) _id
      )
    $ needAnyKey .~ (board^.visibleOnBoardCards == Showdown && updatedPlayerState == Playing)
    $ board
  return ()

shareBoard :: Connections -> Board -> IO ()
shareBoard connections board = mapM_ (sendBoard board) $ Map.toList connections

recvAction :: Socket -> IO PlayerAction
recvAction socket = do
  message <- try $ recv socket 512
  case message of
    Left (exception :: IOException) -> do
                                         putStrLn $ "Exception while receiving action from "
                                                 ++ show socket ++ ": " ++ show exception
                                         putStrLn $ "Player from " ++ show socket ++ " disconnected"
                                         return Quit
    Right action                    -> return $ decode action

finishRound :: Int -> Connections -> Board -> IO Board
finishRound firstPlayerId connections board = do
  let handValues = BU.getHandValues board
  let finalBoard = BU.giveMoney
                 $ banks               .~ (BU.fillBanks board)^.banks
                 $ visibleOnBoardCards .~ Showdown
                 $ activePlayerId      .~ -1
                 $ players             %~ Map.map (\p -> playerBet       .~ 0
                                                       $ playerHandValue .~ (if p^.isInGame
                                                                             then
                                                                               Just $ handValues Map.! (p^.playerId)
                                                                             else
                                                                               Nothing
                                                                            )
                                                       $ p
                                                  )
                 $ board

  shareBoard connections finalBoard
  mapM_ recvAction connections

  return $ BU.kickPlayers finalBoard

finishGame :: Connections -> Board -> IO ()
finishGame connections board = do
  shareBoard connections board
  let (_id, lastPlayer) = head . Map.toList $ board^.players
  putStrLn $ "Winner is " ++ lastPlayer^.playerName ++ " with id " ++ show _id
