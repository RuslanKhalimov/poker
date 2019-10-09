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

import Board      (Board (..), Hand(Showdown), Player (..))
import BoardUtils ( createBoard, fillBanks, getCardSets, getNextId, getNextPlayerId, giveMoney
                  , hideCards, isGameFinished, isRoundFinished, kickPlayers, mergeBoards)
import CardUtils  (handValueFromCardSet)

type Connections = Map.Map Int Socket

main :: IO()
main = withSocketsDo $ do
  args <- getArgs
  (port, playersCount) <- case args of port:playersCount:[] -> return (port, read playersCount)
                                       _                    -> do
                                                                 putStrLn "IncorrectArguments"
                                                                 putStrLn "Excpected <port> <playersCount>"
                                                                 exitFailure

  initialBoard <- createBoard 0 playersCount

  addr <- resolve port
  (TOD sec _) <- getClockTime
  bracket (open addr playersCount) close (runServer ("saves/" ++ show sec ++ ".txt") initialBoard)

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
  connections <- mapM (waitPlayerConnection sock) [0..playersCount board - 1]
  gameLoop filePath 0 (Map.fromList connections) board

waitPlayerConnection :: Socket -> Int -> IO (Int, Socket)
waitPlayerConnection sock _id = do
  (clientSock, addr) <- accept sock
  putStrLn $ show addr ++ " connected"
  return (_id, clientSock)

appendToFile :: FilePath -> Board -> IO ()
appendToFile filePath board = do
  appendFile filePath $ show board
  appendFile filePath "\n"

gameLoop :: FilePath -> Int -> Connections -> Board -> IO ()
gameLoop filePath firstPlayerId connections board = do
  mapM_ (sendBoard True board) $ Map.toList connections
  receivedBoard <- recvBoard (connections Map.! activePlayerId board)
  let mergedBoard = mergeBoards receivedBoard board
  let newBoard    = mergedBoard { activePlayerId = getNextPlayerId mergedBoard }
  appendToFile filePath newBoard
  if isRoundFinished newBoard
  then do
    finalBoard <- finishRound firstPlayerId connections newBoard
    appendToFile filePath finalBoard
    if isGameFinished finalBoard
    then
      finishGame filePath connections
    else
      gameLoop filePath (getNextId firstPlayerId finalBoard) connections finalBoard
  else
    if stepsInRound newBoard == playersCount newBoard
    then do
      let finalBoard = newBoard { visibleOnBoardCards = succ $ visibleOnBoardCards newBoard
                                , stepsInRound        = 0
                                , banks               = banks $ fillBanks newBoard
                                , players             = Map.map (\p -> p { playerBet = 0 }) (players newBoard)
                                }
      appendToFile filePath finalBoard
      gameLoop filePath firstPlayerId connections finalBoard
    else
      gameLoop filePath firstPlayerId connections newBoard

sendBoard :: Bool -> Board -> (Int, Socket) -> IO ()
sendBoard needHide board (_id, sock) = sendAll sock
                                     . encode
                                     . (if needHide
                                       then hideCards (visibleOnBoardCards board) _id
                                       else (\b -> b { needAction = False, needAnyKey = True }))
                                     $ board

recvBoard :: Socket -> IO Board
recvBoard sock = fmap decode $ recv sock 512

finishRound :: Int -> Connections -> Board -> IO Board
finishRound firstPlayerId connections board = do
  let cardSets   = getCardSets board
  let handValues = Map.map handValueFromCardSet cardSets
  let finalBoard = kickPlayers
                 . giveMoney handValues (banks board)
                 $ board { banks               = banks $ fillBanks board
                         , visibleOnBoardCards = Showdown
                         , activePlayerId      = -1
                         , players             = Map.map (\p -> p { playerBet = 0 }) (players board)
                         }

  mapM_ (sendBoard False finalBoard) $ Map.toList connections
  mapM_ recvBoard connections

  newBoard <- createBoard (getNextId firstPlayerId board) (playersCount finalBoard)
  return newBoard { players = Map.map (\p -> p { playerMoney = playerMoney $ players finalBoard Map.! playerId p }) (players newBoard) }

finishGame :: FilePath -> Connections -> IO ()
finishGame filePath connections = do
  return ()
