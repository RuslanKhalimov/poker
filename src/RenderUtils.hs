{-# LANGUAGE TupleSections #-}

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

import           Control.Lens   ((^.))
import qualified Data.Map       as Map
import qualified Data.Set       as Set
import           Data.Foldable  (fold)
import qualified Graphics.Gloss as Gloss
import           Network.Socket (Socket)

import Board      ( Bank, Board, Hand(Showdown), Player, Players, PlayerState(..), activePlayerId, banks, bankMoney
                  , bankParticipants, currentBet, isInGame, onBoardCards, needAction, needAnyKey, playerBet
                  , playerCards, playerHandValue, playerId, playerMoney, playerName, players, playerState, timer
                  , visibleOnBoardCards)
import BoardUtils (getFromActivePlayer)
import Card       (Card, HandValue)
import CardUtils  (fileNameFromCard, fileNameFromCardValue)

type Images = Map.Map String Gloss.Picture

controlPanelColor = makeColor255 170 188 204
deskColor         = makeColor255 25 111 40
backgroundColor   = makeColor255 29 39 48

makeColor255 :: Float -> Float -> Float -> Gloss.Color
makeColor255 r g b = Gloss.makeColor (r / 255) (g / 255) (b / 255) 1

(borderWidth, borderHeight)             = (70, 70)
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
renderInt color x y = renderStringCenter color x y . show

renderString :: Gloss.Color -> Float -> Float -> String -> Gloss.Picture
renderString color x y text = Gloss.translate x y
                            . Gloss.scale 0.1 0.1
                            . Gloss.color color
                            . Gloss.text
                            $ text

renderStringCenter :: Gloss.Color -> Float -> Float -> String -> Gloss.Picture
renderStringCenter color x y text = renderString color getX y text
  where
    getX :: Float
    getX = x - 3.5 * (fromIntegral $ length text)

renderRectangle :: Gloss.Color -> Float -> Float -> Gloss.Picture
renderRectangle color width height = Gloss.color color
                                   . Gloss.polygon
                                   $ Gloss.rectanglePath width height

renderEmptyRectangle ::Float -> Float ->  Gloss.Color -> Gloss.Picture
renderEmptyRectangle width height color = Gloss.color color
                                        . Gloss.lineLoop
                                        $ Gloss.rectanglePath width height

imagesDirectory :: FilePath
imagesDirectory = "images/"

imagesExtension :: String
imagesExtension = ".bmp"

loadImage :: String -> IO Gloss.Picture
loadImage name = Gloss.loadBMP $ imagesDirectory ++ name ++ imagesExtension

getImages :: IO Images
getImages = do
  cardBack   <- imageEntry "cardBack"
  bet        <- imageEntry "bet"
  check      <- imageEntry "check"
  foldButton <- imageEntry "fold"
  cards      <- mapM imageEntry . map (fileNameFromCardValue . toEnum) $ [0..51]

  return . Map.fromList $ [cardBack, bet, check, foldButton] ++ cards
    where
      imageEntry :: String -> IO (String, Gloss.Picture)
      imageEntry name = fmap (name, ) $ loadImage name

renderWorld :: (Socket, Board) -> IO Gloss.Picture
renderWorld (_, board) = do
  images <- getImages
  return $ Gloss.pictures [renderBoard images board, renderControlPanel images board]

renderControlPanel :: Images -> Board -> Gloss.Picture
renderControlPanel images board =
  let controlPanelPicture = renderRectangle controlPanelColor controlPanelWidth controlPanelHeight
      currentBetPicture   = renderInt Gloss.black 0 0 $ board^.currentBet
      messagePicture      = renderMessage board
      timerPicture        = renderTimer board
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
  let banksPicture        = renderBanks $ board^.banks
      deskPicture         = renderRectangle deskColor deskWidth deskHeight
      onBoardCardsPicture = renderOnBoardCards images $ board^.onBoardCards
      playersPicture      = renderPlayers images $ board^.players
      selectingPicture    = renderSelectingRect board
      winnersHighlighting = renderWinnersHighlighting board
  in renderComponent 0 (controlPanelHeight / 2) [ deskPicture
                                                , banksPicture
                                                , onBoardCardsPicture
                                                , selectingPicture
                                                , playersPicture
                                                , winnersHighlighting
                                                ]

renderBanks :: [Bank] -> Gloss.Picture
renderBanks = Gloss.pictures . zipWith ($) banksBuilder . map (^.bankMoney)

renderMessage :: Board -> Gloss.Picture
renderMessage board
  | board^.playerState == Loser  = getMessagePicture "YOU LOST ALL YOUR MONEY :("
  | board^.playerState == Winner = getMessagePicture "YOU ARE WINNER"
  | board^.needAction            = getMessagePicture "YOUR TURN"
  | board^.needAnyKey            = getMessagePicture "PRESS ANY KEY"
  | board^.activePlayerId == -1  = getMessagePicture "WAITING FOR OTHERS"
  | otherwise                    = getMessagePicture $ "WAITING FOR " ++ getFromActivePlayer (^.playerName) board
    where
      getMessagePicture :: String -> Gloss.Picture
      getMessagePicture message = renderString Gloss.black (-deskWidth / 2 + 50) 0 message

renderTimer :: Board -> Gloss.Picture
renderTimer board = if board^.playerState == Playing
                    then
                      renderInt Gloss.black (-deskWidth / 2 + 20) 0 . round $ board^.timer
                    else
                      Gloss.blank

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
renderSelectingRect board = playerCardsBuilder (board^.activePlayerId)
                          $ renderRectangle (Gloss.light deskColor) (2*cardWidth) cardHeight

renderWinnersHighlighting :: Board -> Gloss.Picture
renderWinnersHighlighting board = highlight
  where
    highlight :: Gloss.Picture
    highlight = if board^.visibleOnBoardCards == Showdown
                then
                  banksHighlight <> playersHighlight
                else
                  Gloss.blank

    colors :: [Gloss.Color]
    colors = [Gloss.green, Gloss.red, Gloss.blue]

    banksHighlightBuilder :: [Gloss.Picture -> Gloss.Picture]
    banksHighlightBuilder = [ Gloss.translate 0      (-55)
                            , Gloss.translate (-100) (-55)
                            , Gloss.translate 100    (-55)
                            ]

    banksHighlight :: Gloss.Picture
    banksHighlight = Gloss.pictures
                   . zipWith ($) banksHighlightBuilder
                   . map (renderEmptyRectangle 50 18 . fst)
                   . zip colors
                   $ board^.banks

    playersHighlight :: Gloss.Picture
    playersHighlight = Gloss.pictures
                     . map highlightPlayer
                     . Map.elems
                     $ board^.players

    playerRectangles :: [Gloss.Picture]
    playerRectangles = zipWith (flip ($)) colors
                     [ renderEmptyRectangle (2*cardWidth + 8) (cardHeight + 8)
                     , renderEmptyRectangle (2*cardWidth)     (cardHeight)
                     , renderEmptyRectangle (2*cardWidth - 8) (cardHeight - 8)
                     ]

    highlightPlayer :: Player -> Gloss.Picture
    highlightPlayer player = playerCardsBuilder (player^.playerId)
                           . Gloss.pictures
                           . map fst
                           . filter (\(_, b) -> Set.member (player^.playerId) $ b^.bankParticipants)
                           . zip playerRectangles
                           $ board^.banks

renderCards :: Images -> Player -> Gloss.Picture
renderCards images player =
  if player^.isInGame
  then let [firstCard, secondCard] = player^.playerCards
           firstCardPicture        = Gloss.translate (-cardWidth / 2) 0 $ images Map.! fileNameFromCard firstCard
           secondCardPicture       = Gloss.translate ( cardWidth / 2) 0 $ images Map.! fileNameFromCard secondCard
       in playerCardsBuilder (player^.playerId)
        . Gloss.pictures
        $ [firstCardPicture, secondCardPicture]
  else
    Gloss.blank

renderPlayerInfo :: Player -> Gloss.Picture
renderPlayerInfo player =
  if player^.isInGame
  then
    let
      [y1, y2, y3, y4] = if player^.playerId == 1
                         then [(-30), (-15), 0, (-cardHeight - 55)]
                         else [0, (-20), (-40), cardHeight + 25]
    in playerInfoBuilder (player^.playerId)
     . Gloss.pictures
     $ [ renderInt          Gloss.white (-cardWidth / 2) y1 $ player^.playerBet
       , renderStringCenter Gloss.white (-cardWidth / 2) y2 $ "BET"
       , renderInt          Gloss.white ( cardWidth / 2) y1 $ player^.playerMoney
       , renderStringCenter Gloss.white ( cardWidth / 2) y2 $ "MONEY"
       , renderStringCenter Gloss.white  0               y3 $ player^.playerName
       , renderHandValue                 0               y4 $ player^.playerHandValue
       ]
  else
    Gloss.blank

renderHandValue :: Float -> Float -> Maybe HandValue -> Gloss.Picture
renderHandValue _ _ Nothing          = Gloss.blank
renderHandValue x y (Just handValue) = renderStringCenter Gloss.white x y $ show handValue

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

playerInfoBuilder :: Int -> (Gloss.Picture -> Gloss.Picture)
playerInfoBuilder 0 = Gloss.translate 0 (-deskHeight / 2 - 20)
playerInfoBuilder 1 = Gloss.translate 0 ( deskHeight / 2 + 35)
playerInfoBuilder 2 = Gloss.translate (-deskWidth / 2 - 20) 0 . Gloss.rotate 90
playerInfoBuilder 3 = Gloss.translate ( deskWidth / 2 + 20) 0 . Gloss.rotate 270
playerInfoBuilder _ = const Gloss.blank
