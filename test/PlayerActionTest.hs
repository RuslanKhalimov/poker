import Test.Hspec

import qualified Data.Map as Map

import Board        (Board(..), Player(..))
import BoardUtils   (createBoardUsingGen, getFromActivePlayer, modifyActivePlayer, modifyPlayer)
import PlayerAction (check, bet, foldCards)

defaultBoard :: Board
defaultBoard = check . check
             $ board { players = Map.map (\p -> p { playerBet = 0, playerMoney = 1000 }) (players board) }
  where
    board = createBoardUsingGen 0 1 4

boardWithGreaterBet :: Board
boardWithGreaterBet = modifyPlayer 1 (addBet 200) defaultBoard

addBet :: Int -> Player -> Player
addBet x player = player { playerBet   = playerBet player + x
                         , playerMoney = playerMoney player - x
                         }

leaveGame :: Player -> Player
leaveGame player = player { isInGame = False }

afterEvent :: Int -> Board -> Board
afterEvent steps board = board { currentBet   = 0
                               , needAction   = False
                               , stepsInRound = steps
                               }

main :: IO ()
main = hspec $ do
  describe "EventsHandler.foldCards" $ do
    it "testing without bets"    $
      foldCards defaultBoard        `shouldBe` (afterEvent 2 . modifyActivePlayer leaveGame $ defaultBoard)
    it "testing withGreater bet" $
      foldCards boardWithGreaterBet `shouldBe` (afterEvent 2 . modifyActivePlayer leaveGame $ boardWithGreaterBet)

  describe "EventsHandler.bet" $ do
    it "testing without bets" $
      bet 100 defaultBoard        `shouldBe` (afterEvent 1 . modifyActivePlayer (addBet 100)  $ defaultBoard)
    it "testing with greater bet" $
      bet 100 boardWithGreaterBet `shouldBe` (afterEvent 3 . modifyActivePlayer (addBet 200)  $ boardWithGreaterBet)
    it "testing with big bet" $
      bet 1000000 defaultBoard    `shouldBe` (afterEvent 1 . modifyActivePlayer (addBet 1000) $ defaultBoard)

  describe "EventsHandler.check" $ do
    it "testing without bets" $
      check defaultBoard        `shouldBe` (afterEvent 3 defaultBoard)
    it "testing with greater bet" $
      check boardWithGreaterBet `shouldBe` (afterEvent 3 . modifyActivePlayer (addBet 200) $ boardWithGreaterBet)