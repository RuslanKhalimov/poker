import Test.Hspec

import           Control.Lens ((^.), (.~), (%~))
import qualified Data.Map as Map

import Board        (Board, Player, currentBet, isInGame, needAction, playerBet, playerMoney, players, stepsInRound)
import BoardUtils   (addBet, createBoardUsingGen, getFromActivePlayer, modifyActivePlayer, modifyPlayer)
import PlayerAction (check, bet, foldCards)

defaultBoard :: Board
defaultBoard = check . check
             $ players %~ Map.map ((playerBet .~ 0) . (playerMoney .~ 1000))
             $ board
  where
    board = createBoardUsingGen 0 1 4

boardWithGreaterBet :: Board
boardWithGreaterBet = modifyPlayer 1 (addBet 200) defaultBoard

afterEvent :: Int -> Board -> Board
afterEvent steps board = currentBet   .~ 0
                       $ needAction   .~ False
                       $ stepsInRound .~ steps
                       $ board

main :: IO ()
main = hspec $ do
  describe "EventsHandler.foldCards" $ do
    it "testing without bets"    $
      foldCards defaultBoard        `shouldBe` (afterEvent 2 . modifyActivePlayer (isInGame .~ False) $ defaultBoard)
    it "testing withGreater bet" $
      foldCards boardWithGreaterBet `shouldBe` (afterEvent 2 . modifyActivePlayer (isInGame .~ False) $ boardWithGreaterBet)

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