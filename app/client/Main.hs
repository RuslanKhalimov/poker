module Main
 ( main
 ) where

import Control.Concurrent.MVar        (MVar, newMVar, putMVar )
import Control.Concurrent             (forkFinally)
import Control.Exception              (SomeException, bracket)
import Control.Lens                   ((^.))
import Control.Monad                  (when)
import Data.Binary                    (encode, decode)
import Network.Socket          hiding (recv, sendAll)
import Network.Socket.ByteString.Lazy (recv, sendAll)
import System.Environment             (getArgs)
import System.Exit                    (exitFailure)

import Graphics (startGame)
import Board    (Board, PlayerState(Playing), playerState)

main :: IO ()
main = withSocketsDo $ do
  args <- getArgs
  (name, ip, port, count) <- case args of
                               [name, ip, port, count] -> return (name, ip, port, read count)
                               _                       -> do
                                                            putStrLn "Incorrect arguments"
                                                            putStrLn "Excpected <name> <ip> <port> <players count>"
                                                            exitFailure

  addr <- resolve ip port
  bracket (open name count addr) close runClient

resolve :: String -> String -> IO AddrInfo
resolve host port = do
  let hints   = defaultHints { addrSocketType = Stream }
  addrInfo:_ <- getAddrInfo (Just hints) (Just host) (Just port)
  return addrInfo

open :: String -> Int -> AddrInfo -> IO Socket
open name count addr = do
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  connect sock (addrAddress addr)
  sendAll sock $ encode name
  sendAll sock $ encode count
  return sock

handleException :: Either SomeException () -> IO ()
handleException (Left exception) = putStrLn $ "Lost connection to server : " ++ show exception
handleException _                = putStrLn "Game finished"

runClient :: Socket -> IO ()
runClient sock = do
  message  <- fmap decode $ recv sock 64
  putStrLn message
  when (message == "Connected") $ do
    putStrLn "Waiting for other players"
    board    <- recvBoard sock
    recvMVar <- newMVar board
    flip forkFinally handleException $ recvLoop recvMVar sock
    startGame (sock, board) recvMVar

recvLoop :: MVar Board -> Socket -> IO ()
recvLoop recvMVar sock = do
  receivedBoard <- recvBoard sock
  putMVar recvMVar receivedBoard
  when (receivedBoard^.playerState == Playing) $ recvLoop recvMVar sock

recvBoard :: Socket -> IO Board
recvBoard sock = fmap decode $ recv sock 512