module Main
 ( main
 ) where

import Control.Concurrent.MVar        (MVar, newMVar, putMVar )
import Control.Concurrent             (forkIO)
import Control.Exception              (bracket)
import Control.Monad                  (forever)
import Data.Binary                    (encode, decode)
import Network.Socket          hiding (recv, sendAll)
import Network.Socket.ByteString.Lazy (recv, sendAll)
import System.Environment             (getArgs)
import System.Exit                    (exitFailure)

import Graphics (startGame)
import Board    (Board)

main :: IO ()
main = withSocketsDo $ do
  args <- getArgs
  (ip, port) <- case args of ip:port:[] -> return (ip, port)
                             _          -> do
                                             putStrLn "IncorrectArguments"
                                             putStrLn "Excpected <ip> <port>"
                                             exitFailure

  addr <- resolve ip port
  bracket (open addr) close runClient

resolve :: String -> String -> IO AddrInfo
resolve host port = do
  let hints   = defaultHints { addrSocketType = Stream }
  addrInfo:_ <- getAddrInfo (Just hints) (Just host) (Just port)
  return addrInfo

open :: AddrInfo -> IO Socket
open addr = do
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  connect sock (addrAddress addr)
  return sock

runClient :: Socket -> IO ()
runClient sock = do
  board    <- recvBoard sock
  recvMVar <- newMVar board
  forkIO $ recvLoop recvMVar sock
  startGame (sock, board) recvMVar

recvLoop :: MVar Board -> Socket -> IO ()
recvLoop recvMVar sock = do
  forever $ do
    receivedBoard <- recvBoard sock
    putMVar recvMVar receivedBoard

recvBoard :: Socket -> IO Board
recvBoard sock = fmap decode $ recv sock 512