module Main
  ( main
  ) where

import           Control.Concurrent             (forkFinally)
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

type Connections = Map.Map Int (String, Socket)

main :: IO()
main = withSocketsDo $ do
  args <- getArgs
  (port, playersCount) <- case args of port:playersCount:[] -> return (port, read playersCount)
                                       _                    -> do
                                                                 putStrLn "IncorrectArguments"
                                                                 putStrLn "Excpected <port> <playersCount>"
                                                                 exitFailure

  initialBoard <- BU.createBoard 0 playersCount

  addr <- resolve port
  (TOD sec _) <- getClockTime
  bracket (open addr playersCount) close (runServer ("saves/" ++ show sec) initialBoard)

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

runServer :: FilePath -> Board -> Socket -> IO ()
runServer filePath board sock = do
  connections <- fmap Map.fromList $ mapM (waitPlayerConnection sock) [0..playersCount board - 1]
  gameLoop filePath 0 connections board { players = Map.map (\p -> p { playerName = fst $ connections Map.! playerId p }) (players board) }

waitPlayerConnection :: Socket -> Int -> IO (Int, (String, Socket))
waitPlayerConnection sock _id = do
  (clientSock, addr) <- accept sock
  name               <- fmap decode $ recv clientSock 32
  putStrLn $ name ++ " from " ++ show addr ++ " connected"
  return (_id, (name, clientSock))

appendToFile :: FilePath -> Board -> IO ()
appendToFile filePath board = do
  appendFile filePath $ show board
  appendFile filePath "\n"

gameLoop :: FilePath -> Int -> Connections -> Board -> IO ()
gameLoop filePath firstPlayerId connections board = do
  mapM_ (sendBoard True board) $ Map.toList connections
  receivedBoard <- recvBoard (connections Map.! activePlayerId board)
  let mergedBoard = BU.mergeBoards receivedBoard board
  let newBoard    = mergedBoard { activePlayerId = BU.getNextPlayerId mergedBoard }
  appendToFile filePath newBoard
  if BU.isRoundFinished newBoard
  then do
    finalBoard <- finishRound firstPlayerId connections newBoard
    appendToFile filePath finalBoard
    if BU.isGameFinished finalBoard
    then
      finishGame filePath connections
    else
      gameLoop filePath (BU.getNextId firstPlayerId finalBoard) connections finalBoard
  else
    if stepsInRound newBoard == playersCount newBoard
    then do
      let finalBoard = newBoard { visibleOnBoardCards = succ $ visibleOnBoardCards newBoard
                                , stepsInRound        = 0
                                , banks               = banks $ BU.fillBanks newBoard
                                , players             = Map.map (\p -> p { playerBet = 0 }) (players newBoard)
                                }
      appendToFile filePath finalBoard
      gameLoop filePath firstPlayerId connections finalBoard
    else
      gameLoop filePath firstPlayerId connections newBoard

sendBoard :: Bool -> Board -> (Int, (String, Socket)) -> IO ()
sendBoard needHide board (_id, (name, sock)) = sendAll sock
                                                   . encode
                                                   . (if needHide
                                                     then BU.hideCards (visibleOnBoardCards board) _id
                                                     else (\b -> b { needAction = False, needAnyKey = True }))
                                                   $ board

recvBoard :: (String, Socket) -> IO Board
recvBoard (_, sock) = fmap decode $ recv sock 512

finishRound :: Int -> Connections -> Board -> IO Board
finishRound firstPlayerId connections board = do
  let cardSets   = BU.getCardSets board
  let handValues = Map.map handValueFromCardSet cardSets
  let finalBoard = BU.kickPlayers
                 . BU.giveMoney handValues (banks board)
                 $ board { banks               = banks $ BU.fillBanks board
                         , visibleOnBoardCards = Showdown
                         , activePlayerId      = -1
                         , players             = Map.map (\p -> p { playerBet = 0 }) (players board)
                         }

  mapM_ (sendBoard False finalBoard) $ Map.toList connections
  mapM_ recvBoard connections

  newBoard <- BU.createBoard (BU.getNextId firstPlayerId board) (playersCount finalBoard)
  return newBoard { players = Map.map (\p -> p { playerMoney = playerMoney $ players finalBoard Map.! playerId p }) (players newBoard) }

finishGame :: FilePath -> Connections -> IO ()
finishGame filePath connections = do
  return ()
