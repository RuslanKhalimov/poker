 module RenderUtils
  ( backgroundColor
  , betButtonX
  , buttonHeight
  , buttonWidth
  , checkButtonX
  , controlPanelY
  , foldButtonX
  , renderWorld
  , screenPos
  , screenSize
  ) where

import qualified Data.Map       as Map
import           Data.Foldable  (fold)
import qualified Graphics.Gloss as Gloss
import           Network.Socket (Socket)

import Board     (Bank (..), Board (..), Player (..), Players)
import Card      (Card)
import CardUtils (fileNameFromCard)

type Images = Map.Map String Gloss.Picture

controlPanelColor = makeColor255 170 188 204
deskColor         = makeColor255 25 111 40
backgroundColor   = makeColor255 29 39 48

makeColor255 :: Float -> Float -> Float -> Gloss.Color
makeColor255 r g b = Gloss.makeColor (r / 255) (g / 255) (b / 255) 1

(borderWidth, borderHeight)             = (50, 50)
(buttonWidth, buttonHeight)             = (100, 43 :: Float)
(cardWidth, cardHeight)                 = (70, 100)
(deskWidth, deskHeight)                 = (700, 400)
(controlPanelWidth, controlPanelHeight) = (2 * borderWidth + deskWidth, 50)

betButtonX    = controlPanelWidth / 2 - 5 * buttonWidth / 2 - 30
checkButtonX  = controlPanelWidth / 2 - 3 * buttonWidth / 2 - 20
foldButtonX   = controlPanelWidth / 2 -     buttonWidth / 2 - 10
controlPanelY = -deskHeight / 2 - borderHeight

screenSize, screenPos :: (Int, Int)
screenSize = (round $ deskWidth + 2 * borderWidth, round $ deskHeight + controlPanelHeight + 2 * borderHeight)
screenPos  = (0, 0)

renderComponent :: Float -> Float -> [Gloss.Picture] -> Gloss.Picture
renderComponent x y components = Gloss.translate x y . Gloss.pictures $ components

renderInt :: Gloss.Color -> Float -> Float -> Int -> Gloss.Picture
renderInt color x y = Gloss.translate x y
                    . Gloss.scale 0.1 0.1
                    . Gloss.color color
                    . Gloss.text
                    . show

renderRectangle :: Gloss.Color -> Float -> Float -> Gloss.Picture
renderRectangle color width height = Gloss.color color
                                   . Gloss.polygon
                                   $ Gloss.rectanglePath width height

renderWorld :: Images -> (Socket, Board) -> IO Gloss.Picture
-- renderWorld (_, board) = return blank
renderWorld images (_, board) = pure (renderBoard images board)
                             <> pure (renderControlPanel images board)

renderControlPanel :: Images -> Board -> Gloss.Picture
renderControlPanel images board =
  let controlPanelPicture = renderRectangle controlPanelColor controlPanelWidth controlPanelHeight
      currentBetPicture   = renderInt Gloss.black 0 0 . currentBet $ board
      messagePicture      = renderMessage board
      timerPicture        = renderInt Gloss.black (-deskWidth / 2 + 20) 0 . round . timer $ board
      betButtonPicture    = Gloss.translate betButtonX   0 $ images Map.! "bet"
      checkButtonPicture  = Gloss.translate checkButtonX 0 $ images Map.! "check"
      foldButtonPicture   = Gloss.translate foldButtonX  0 $ images Map.! "fold"
  in renderComponent 0 (-deskHeight / 2 - borderHeight) [ controlPanelPicture
                                                        , currentBetPicture
                                                        , betButtonPicture
                                                        , checkButtonPicture
                                                        , foldButtonPicture
                                                        , messagePicture
                                                        , timerPicture
                                                        ]

renderBoard ::Images -> Board -> Gloss.Picture
renderBoard images board =
  let banksPicture        = renderBanks (banks board)
      deskPicture         = renderRectangle deskColor deskWidth deskHeight
      onBoardCardsPicture = renderOnBoardCards images (onBoardCards board)
      playersPicture      = renderPlayers images (players board)
      selectingPicture    = renderSelectingRect board
  in renderComponent 0 (controlPanelHeight / 2) [ deskPicture
                                                , banksPicture
                                                , onBoardCardsPicture
                                                , selectingPicture
                                                , playersPicture
                                                ]

renderBanks :: [Bank] -> Gloss.Picture
renderBanks = Gloss.pictures . zipWith ($) banksBuilder . map money

renderMessage :: Board -> Gloss.Picture
renderMessage board
  | needAction board = getMessagePicture "YOUR TURN"
  | needAnyKey board = getMessagePicture "PRESS ANY KEY"
  | otherwise        = getMessagePicture "WAIT OTHER PLAYERS"
    where
      getMessagePicture :: String -> Gloss.Picture
      getMessagePicture message = Gloss.translate (-deskWidth / 2 + 50) 0
                                . Gloss.scale 0.1 0.1
                                . Gloss.color Gloss.black
                                . Gloss.text
                                $ message

renderOnBoardCards :: Images -> [Card] -> Gloss.Picture
renderOnBoardCards images cards = Gloss.pictures
                                . zipWith ($) onBoardCardsBuilder
                                . map ((images Map.! ) . fileNameFromCard)
                                $ cards

renderPlayers :: Images -> Players -> Gloss.Picture
renderPlayers images players = Gloss.pictures
                             $ (Map.elems . Map.map (renderCards images) $ players)
                             ++ (Map.elems . Map.map renderPlayerInfo $ players)


renderSelectingRect :: Board -> Gloss.Picture
renderSelectingRect board = playerCardsBuilder (activePlayerId board)
                          $ renderRectangle (Gloss.light deskColor) (2*cardWidth) cardHeight

renderCards :: Images -> Player -> Gloss.Picture
renderCards images player =
  if isInGame player
  then let [firstCard, secondCard] = playerCards player
           firstCardPicture        = Gloss.translate (-cardWidth / 2) 0 $ images Map.! fileNameFromCard firstCard
           secondCardPicture       = Gloss.translate ( cardWidth / 2) 0 $ images Map.! fileNameFromCard secondCard
       in playerCardsBuilder (playerId player)
        . Gloss.pictures
        $ [firstCardPicture, secondCardPicture]
  else
    Gloss.blank

renderPlayerInfo :: Player -> Gloss.Picture
renderPlayerInfo player =
  if isInGame player
  then playerInfoBuilder (playerId player)
     . Gloss.pictures
     $ [ renderInt Gloss.white (-40) 0 $ playerBet   player
       , renderInt Gloss.white   20  0 $ playerMoney player
       ]
  else
    Gloss.blank

banksBuilder :: [Int -> Gloss.Picture]
banksBuilder = [ renderInt Gloss.white 0      (-60)
               , renderInt Gloss.white (-100) (-60)
               , renderInt Gloss.white 100    (-60)
               ]

onBoardCardsBuilder :: [Gloss.Picture -> Gloss.Picture]
onBoardCardsBuilder = [ Gloss.translate (-2*cardWidth) 0
                      , Gloss.translate (-cardWidth) 0
                      , Gloss.translate 0 0
                      , Gloss.translate cardWidth 0
                      , Gloss.translate (2*cardWidth) 0
                      ]

playerCardsBuilder :: Int -> (Gloss.Picture -> Gloss.Picture)
playerCardsBuilder 0 = Gloss.translate 0 (cardHeight / 2 - deskHeight / 2)
playerCardsBuilder 1 = Gloss.translate 0 (deskHeight / 2 - cardHeight / 2)
playerCardsBuilder 2 = Gloss.translate (cardHeight / 2 - deskWidth / 2) 0 . Gloss.rotate 90
playerCardsBuilder 3 = Gloss.translate (deskWidth / 2 - cardHeight / 2) 0 . Gloss.rotate 90
playerCardsBuilder _ = const Gloss.blank

playerInfoBuilder :: Int -> (Gloss.Picture ->  Gloss.Picture)
playerInfoBuilder 0 = Gloss.translate 0 (cardHeight - deskHeight / 2 + 5)
playerInfoBuilder 1 = Gloss.translate 0 (deskHeight / 2 - cardHeight - 15)
playerInfoBuilder 2 = Gloss.translate (cardHeight - deskWidth / 2 + 10) 0 . Gloss.rotate 90
playerInfoBuilder 3 = Gloss.translate (deskWidth / 2 - cardHeight - 10) 0 . Gloss.rotate 270
playerInfoBuilder _ = const Gloss.blank
