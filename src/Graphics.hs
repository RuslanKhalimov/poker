{-# LANGUAGE TupleSections #-}

module Graphics
  ( startGame
  ) where

import           Control.Concurrent.MVar        (MVar, tryTakeMVar)
import           Control.Lens                   ((^.), (%~))
import           Data.Binary                    (encode)
import qualified Data.Map                         as Map
import           Data.Maybe                     (fromMaybe)
import qualified Graphics.Gloss                   as Gloss
import qualified Graphics.Gloss.Interface.IO.Game as Game
import           Network.Socket                 (Socket)
import           Network.Socket.ByteString.Lazy (sendAll)

import Board         (Board, needAction, needAnyKey, timer)
import BoardUtils    (modifyActivePlayer)
import CardUtils     (fileNameFromCardValue)
import EventsHandler (handleEvent, waitAnyKey)
import PlayerAction  (PlayerAction(Fold))
import RenderUtils   ( backgroundColor, betButtonX, buttonHeight, buttonWidth, checkButtonX
                     , controlPanelY, foldButtonX, renderWorld, screenPos, screenSize)

type World = (Socket, Board)

displayMode :: Gloss.Display
displayMode = (Gloss.InWindow "Poker" screenSize screenPos)

fps :: Int
fps = 5

imagesDirectory :: FilePath
imagesDirectory = "images/"

imagesExtension :: String
imagesExtension = ".bmp"

loadImage :: String -> IO Gloss.Picture
loadImage name = Gloss.loadBMP $ imagesDirectory ++ name ++ imagesExtension

images :: IO (Map.Map String Gloss.Picture)
images = do
  cardBack   <- imageEntry "cardBack"
  bet        <- imageEntry "bet"
  check      <- imageEntry "check"
  foldButton <- imageEntry "fold"
  cards      <- mapM imageEntry . map (fileNameFromCardValue . toEnum) $ [0..51]

  return . Map.fromList $ [cardBack, bet, check, foldButton] ++ cards
    where
      imageEntry :: String -> IO (String, Gloss.Picture)
      imageEntry name = fmap (name, ) $ loadImage name

startGame :: World -> MVar Board -> IO ()
startGame world recvMVar = do
  imagesMap <- images
  Game.playIO displayMode backgroundColor fps world (renderWorld imagesMap) handler (updater recvMVar)

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

handle :: (Game.Event -> Board -> (Board, Maybe PlayerAction)) -> Game.Event -> World -> IO World
handle callback event (sock, board) = do
  let (newBoard, action) = callback event board
  case action of
    Nothing -> return (sock, newBoard)
    Just ac -> do
                 sendAll sock $ encode ac
                 return (sock, newBoard)

updater :: MVar Board -> Float -> World -> IO World
updater recvMVar time (sock, board) = do
  receivedBoard <- tryTakeMVar recvMVar
  let updatedBoard = fromMaybe (timer %~ (time `subtract`) $ board) receivedBoard
  if board^.needAction && board^.timer < 0
  then do
    sendAll sock $ encode Fold
    return (sock, updatedBoard)
  else
    return (sock, updatedBoard)
