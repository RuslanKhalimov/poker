{-# LANGUAGE TupleSections #-}

module Main
  ( main
  ) where

import           Control.Concurrent             (forkIO)
import           Control.Concurrent.MVar        (MVar, newMVar, putMVar, takeMVar)
import           Control.Exception              (bracket)
import           Data.Binary                    (encode, decode)
import qualified Data.Map as Map
import           Network.Socket          hiding (recv, sendAll)
import           Network.Socket.ByteString.Lazy (recv, sendAll)
import           System.Environment             (getArgs)
import           System.Exit                    (exitFailure)
import           System.Time                    (ClockTime(TOD), getClockTime)

import           Board     (Board (..), Hand(Showdown), Player (..))
import qualified BoardUtils as BU
import           CardUtils (handValueFromCardSet)

type Connection = (Int, Socket)
type SharedInfo = MVar (Bool, Board)

main :: IO()
main = withSocketsDo $ do
  args                 <- getArgs
  (port, playersCount) <- case args of port:playersCount:[] -> return (port, read playersCount)
                                       _                    -> do
                                                                 putStrLn "IncorrectArguments"
                                                                 putStrLn "Excpected <port> <playersCount>"
                                                                 exitFailure

  initialBoard <- BU.createBoard 0 playersCount

  addr <- resolve port
  bracket (open addr playersCount) close (runServer initialBoard)

resolve :: String -> IO AddrInfo
resolve port = do
  let hints = defaultHints { addrFlags = [AI_PASSIVE]
                           , addrSocketType = Stream
                           }
  addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
  return addr

open :: AddrInfo -> Int -> IO Socket
open addr playersCount = do
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  setSocketOption sock ReuseAddr playersCount
  bind sock (addrAddress addr)
  listen sock playersCount
  return sock

talkWithClient :: SharedInfo -> Connection -> IO ()
talkWithClient boardMVar connection = do
  (needHide, board) <- takeMVar boardMVar
  sendBoard needHide board connection
  talkWithClient boardMVar connection

runServer :: Board -> Socket -> IO ()
runServer board sock = do
  playersInfo    <- mapM (waitPlayerConnection sock) [0..playersCount board - 1]
  let connections = map snd $ playersInfo
  let names       = map fst playersInfo
  let newBoard    = board { players = Map.map (\p -> p { playerName = names !! playerId p }) (players board) }
  clientMVars    <- mapM (newMVar . (True,)) $ replicate (playersCount newBoard) newBoard
  mapM_ (forkIO . (_talkWithClient clientMVars)) connections
  gameLoop 0 connections clientMVars newBoard
    where
      _talkWithClient :: [SharedInfo] -> Connection -> IO ()
      _talkWithClient clientMVars connection@(_id, _) = talkWithClient (clientMVars !! _id) connection

waitPlayerConnection :: Socket -> Int -> IO (String, Connection)
waitPlayerConnection sock _id = do
  (clientSock, addr) <- accept sock
  name               <- fmap decode $ recv clientSock 32
  putStrLn $ name ++ " from " ++ show addr ++ " connected"
  return (name, (_id, clientSock))

shareBoard :: Bool -> [Connection] -> [SharedInfo] -> Board -> IO ()
shareBoard needHide connections clientMVars board = do
  mapM_ (\_id -> putMVar (clientMVars !! _id) (needHide, board)) $ map fst connections

gameLoop :: Int -> [Connection] -> [SharedInfo] -> Board -> IO ()
gameLoop firstPlayerId connections clientMVars board = do
  shareBoard True connections clientMVars board

  receivedBoard <- recvBoard (map snd connections !! activePlayerId board)
  let newBoard = BU.mergeBoards receivedBoard board
  if BU.isRoundFinished newBoard
  then do
    finalBoard <- finishRound firstPlayerId connections clientMVars newBoard
    if BU.isGameFinished finalBoard
    then
      finishGame connections clientMVars finalBoard
    else
      gameLoop (BU.getNextId firstPlayerId finalBoard) connections clientMVars finalBoard
  else
    if stepsInRound newBoard == playersCount newBoard
    then do
      let finalBoard = newBoard { visibleOnBoardCards = succ $ visibleOnBoardCards newBoard
                                , stepsInRound        = 0
                                , banks               = banks $ BU.fillBanks newBoard
                                , players             = Map.map (\p -> p { playerBet = 0 }) (players newBoard)
                                }
      gameLoop firstPlayerId connections clientMVars finalBoard
    else
      gameLoop firstPlayerId connections clientMVars newBoard { activePlayerId = BU.getNextPlayerId newBoard }

sendBoard :: Bool -> Board -> Connection -> IO ()
sendBoard needHide board (_id, sock) = sendAll sock
                                     . encode
                                     . (if needHide
                                     then BU.hideCards (visibleOnBoardCards board) _id
                                     else (\b -> b { needAction = False, needAnyKey = True }))
                                     $ board

recvBoard :: Socket -> IO Board
recvBoard sock = fmap decode $ recv sock 512

finishRound :: Int -> [Connection] -> [SharedInfo] -> Board -> IO Board
finishRound firstPlayerId connections clientMVars board = do
  let cardSets   = BU.getCardSets board
  let handValues = Map.map handValueFromCardSet cardSets
  let finalBoard = BU.kickPlayers
                 . BU.giveMoney handValues
                 $ board { banks               = banks $ BU.fillBanks board
                         , visibleOnBoardCards = Showdown
                         , activePlayerId      = -1
                         , players             = Map.map (\p -> p { playerBet = 0 }) (players board)
                         }

  shareBoard False connections clientMVars finalBoard
  mapM_ recvBoard $ map snd connections

  (TOD time _) <- getClockTime
  return $ BU.nextDeal time (BU.getNextId firstPlayerId board) False finalBoard

finishGame :: [Connection] -> [SharedInfo] -> Board -> IO ()
finishGame connections clientMVars board = do
  shareBoard False connections clientMVars board
