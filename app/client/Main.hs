module Main
 ( main
 ) where

import Control.Concurrent.MVar        (MVar, newMVar, putMVar )
import Control.Concurrent             (forkFinally)
import Control.Exception              (SomeException, bracket)
import Control.Monad                  (forever)
import Data.Binary                    (encode, decode)
import Network.Socket          hiding (recv, sendAll)
import Network.Socket.ByteString.Lazy (recv, sendAll)
import System.Environment             (getArgs)
import System.Exit                    (exitFailure)

import Graphics (startGame)
import Board    (Board, activePlayerId)

main :: IO ()
main = withSocketsDo $ do
  args <- getArgs
  (name, ip, port) <- case args of
                              [name, ip, port] -> return (name, ip, port)
                              _                      -> do
                                                          putStrLn "IncorrectArguments"
                                                          putStrLn "Excpected <name> <ip> <port>"
                                                          exitFailure

  addr <- resolve ip port
  bracket (open name addr) close runClient

resolve :: String -> String -> IO AddrInfo
resolve host port = do
  let hints   = defaultHints { addrSocketType = Stream }
  addrInfo:_ <- getAddrInfo (Just hints) (Just host) (Just port)
  return addrInfo

open :: String -> AddrInfo -> IO Socket
open name addr = do
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  connect sock (addrAddress addr)
  sendAll sock $ encode name
  return sock

handleException :: Either SomeException () -> IO ()
handleException (Left exception) = do
                                     putStrLn "Exception while receiving board"
                                     putStrLn $ show exception
handleException _                = putStrLn "Game finisghed"

runClient :: Socket -> IO ()
runClient sock = do
  board    <- recvBoard sock
  recvMVar <- newMVar board
  flip forkFinally handleException $ recvLoop recvMVar sock
  startGame (sock, board) recvMVar

recvLoop :: MVar Board -> Socket -> IO ()
recvLoop recvMVar sock = do
  forever $ do
    receivedBoard <- recvBoard sock
    putMVar recvMVar receivedBoard

recvBoard :: Socket -> IO Board
recvBoard sock = fmap decode $ recv sock 512