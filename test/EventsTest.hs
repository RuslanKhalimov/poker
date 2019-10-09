import Test.Hspec

import qualified Data.Map as M
import System.Random (mkStdGen)

import Board
import BoardUtils (createBoardUsingGen, modifyActivePlayer, modifyPlayer)
import EventsHandler (bet, foldCards)

defaultBoard :: Board
defaultBoard = (createBoardUsingGen (mkStdGen 0) 4) { currentBet = 100 }

addBet :: Int -> Player -> Player
addBet x player = player { playerBet   = playerBet player + x
                         , playerMoney = playerMoney player - x
                         }

boardWithGreaterBet :: Board
boardWithGreaterBet = modifyPlayer 1 (addBet 200) defaultBoard { maxBet = 200 }

boardWithLowerBet :: Board
boardWithLowerBet   = modifyPlayer 1 (addBet 50)  defaultBoard { maxBet = 50 }

leaveGame :: Player -> Player
leaveGame player = player { isInGame = False }

afterEvent :: Bool -> Board -> Board
afterEvent isBet board = board { currentBet   = 0
                               , needAction   = False
                               , stepsInRound = if isBet then 1 else stepsInRound board + 1
                               }

main :: IO ()
main = hspec $ do
  describe "EventsHandler.foldCards" $ do
    it "testing without bets" $
      foldCards defaultBoard        `shouldBe` afterEvent False (modifyActivePlayer (leaveGame) defaultBoard)
    it "testing withGreater bet" $
      foldCards boardWithGreaterBet `shouldBe` afterEvent False (modifyActivePlayer (leaveGame) boardWithGreaterBet)

  describe "EventsHandler.bet" $ do
    it "testing without bets" $
      bet defaultBoard        `shouldBe` afterEvent True  (modifyActivePlayer (addBet 100) defaultBoard      { maxBet = 100 })
    it "testing with lower bet" $
      bet boardWithLowerBet   `shouldBe` afterEvent True  (modifyActivePlayer (addBet 100) boardWithLowerBet { maxBet = 100 })
    it "testing with greater bet" $
      bet boardWithGreaterBet `shouldBe` afterEvent False (modifyActivePlayer (addBet 200) boardWithGreaterBet)