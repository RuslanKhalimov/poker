module Graphics
  ( startGame
  ) where

import           Control.Concurrent.MVar        (MVar, tryTakeMVar)
import           Control.Lens                   ((^.), (%~))
import           Control.Monad                  (when)
import           Control.Monad.State            (State, runState)
import           Data.Binary                    (encode)
import qualified Data.Map                         as Map
import           Data.Maybe                     (fromMaybe)
import qualified Graphics.Gloss                   as Gloss
import qualified Graphics.Gloss.Interface.IO.Game as Game
import           Network.Socket                 (Socket)
import           Network.Socket.ByteString.Lazy (sendAll)

import Board         (Board, needAction, needAnyKey, timer)
import BoardUtils    (modifyActivePlayer)
import EventsHandler (handleEvent, waitAnyKey)
import PlayerAction  (PlayerAction(Fold, Ok))
import RenderUtils   ( backgroundColor, betButtonX, buttonHeight, buttonWidth, checkButtonX
                     , controlPanelY, foldButtonX, renderWorld, screenPos, screenSize)

type World = (Socket, Board)

displayMode :: Gloss.Display
displayMode = (Gloss.InWindow "Poker" screenSize screenPos)

fps :: Int
fps = 5

startGame :: World -> MVar Board -> IO ()
startGame world recvMVar = Game.playIO displayMode backgroundColor fps world renderWorld handler (updater recvMVar)

handler :: Game.Event -> World -> IO World
handler event world@(_, board) =
  if board^.needAction
  then
    handle handleEvent event world
  else if board^.needAnyKey
  then
    handle waitAnyKey event world
  else
    return world

handle :: (Game.Event -> State Board (Maybe PlayerAction)) -> Game.Event -> World -> IO World
handle callback event (sock, board) = do
  let (action, newBoard) = runState (callback event) board
  case action of
    Nothing -> return (sock, newBoard)
    Just ac -> do
                 sendAll sock $ encode ac
                 return (sock, newBoard)

updater :: MVar Board -> Float -> World -> IO World
updater recvMVar time (sock, board) = do
  receivedBoard <- tryTakeMVar recvMVar
  let updatedBoard = fromMaybe (timer %~ (time `subtract`) $ board) receivedBoard
  when (board^.needAction && board^.timer < 0) $ sendAll sock $ encode Fold
  when (board^.needAnyKey && board^.timer < 0) $ sendAll sock $ encode Ok
  return (sock, updatedBoard)
