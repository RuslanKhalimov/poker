import Test.Hspec

import qualified Data.Map as Map
import qualified Data.Set as Set

import Board        (Bank(..), Board(..), Player(..))
import BoardUtils   (addBet, createBoardUsingGen, fillBanks, giveMoney, modifyPlayer)
import Card         (Card(..), CardValue(..), Rank(..), Suit(..))

main :: IO ()
main = hspec $ do
  describe "BoardUtils.fillBanks" $ do
    it "no banks" $
      fillBanks defaultBoard           `shouldBe` defaultBoard
    it "one bank" $
      fillBanks boardWithBets          `shouldBe` boardWithOneBank
    it "two banks" $
      fillBanks boardWithDifferentBets `shouldBe` boardWithTwoBanks
    it "left player" $
      fillBanks boardWithLeftPlayer    `shouldBe`  boardWithTwoBanks { players = Map.adjust leavePlayer 1
                                                                               $ players boardWithTwoBanks
                                                                     }

  describe "BoardUtils.giveMoney (no winner)" $ do
    it "no banks" $
      giveMoney boardWithEqualHandValues `shouldBe` boardWithEqualHandValues
    it "one bank" $
      giveMoney boardWithOneBank         `shouldBe` boardWithEqualHandValues
    it "two banks" $
      giveMoney boardWithTwoBanks        `shouldBe` boardWithEqualHandValues

  describe "BoardUtils.giveMoney (with winner)" $ do
    it "one winner" $
      giveMoney boardWithOneWinner  `shouldBe` boardWithOneWinner  { players = Map.adjust (setPlayerMoney 1400) 0
                                                                             $ players boardWithOneWinner
                                                                   , banks   = [initialBank]
                                                                   }
    it "two winners" $
      giveMoney boardWithTwoWinners `shouldBe` boardWithTwoWinners { players = Map.adjust (setPlayerMoney 1000) 0
                                                                             . Map.adjust (setPlayerMoney 1300) 2
                                                                             $ players boardWithTwoWinners
                                                                   , banks   = [initialBank]
                                                                   }

initialBank :: Bank
initialBank = Bank { money        = 0
                   , participants = Set.fromList [0, 1, 2, 3]
                   }

setPlayerMoney :: Int -> Player -> Player
setPlayerMoney x player = player { playerMoney = x }

setPlayerBet :: Int -> Player -> Player
setPlayerBet x player = player { playerBet = x }

setPlayerCards :: [Card] -> Player -> Player
setPlayerCards cards player = player { playerCards = cards }

leavePlayer :: Player -> Player
leavePlayer player = player { isInGame = False }

defaultBoard :: Board
defaultBoard = board { players = Map.map (setPlayerBet 0 . setPlayerMoney 1000) $ players board }
  where
    board :: Board
    board = createBoardUsingGen 0 1 4

boardWithEqualHandValues :: Board
boardWithEqualHandValues = defaultBoard { onBoardCards = [ Card $ CardValue Ace   Spades
                                                         , Card $ CardValue King  Spades
                                                         , Card $ CardValue Jack  Spades
                                                         , Card $ CardValue Queen Spades
                                                         , Card $ CardValue Ten   Spades
                                                         ]
                                        }

boardWithBets :: Board
boardWithBets = boardWithEqualHandValues { players = Map.map (addBet 100)
                                                   $ players boardWithEqualHandValues
                                         }

boardWithDifferentBets :: Board
boardWithDifferentBets = boardWithBets { players = Map.adjust (addBet 100) 0
                                                 . Map.adjust (addBet 100) 1
                                                 $ players boardWithBets
                                       }

boardWithLeftPlayer :: Board
boardWithLeftPlayer = boardWithDifferentBets { players = Map.adjust leavePlayer 1 $ players boardWithDifferentBets }

boardWithOneBank :: Board
boardWithOneBank = boardWithEqualHandValues { banks   = [bank]
                                            , players = Map.map (setPlayerMoney 900)
                                                      $ players boardWithEqualHandValues
                                            }
  where
    bank :: Bank
    bank = Bank { money        = 400
                , participants = Map.keysSet $ players boardWithEqualHandValues
                }

boardWithTwoBanks :: Board
boardWithTwoBanks = boardWithOneBank { banks   = bank : banks boardWithOneBank
                                     , players = Map.adjust (setPlayerMoney 800) 0
                                               . Map.adjust (setPlayerMoney 800) 1
                                               $ players boardWithOneBank
                                     }
  where
    bank :: Bank
    bank = Bank { money        = 200
                , participants = Set.fromList [0, 1]
                }

boardWithOneWinner :: Board
boardWithOneWinner = boardWithTwoBanks { onBoardCards = [ Card $ CardValue Ace   Spades
                                                        , Card $ CardValue King  Spades
                                                        , Card $ CardValue Ace   Diamonds
                                                        , Card $ CardValue King  Diamonds
                                                        , Card $ CardValue Two   Spades
                                                        ]
                                       , players      = Map.adjust (setPlayerCards [ Card $ CardValue King Clubs
                                                                                   , Card $ CardValue King Hearts
                                                                                   ]
                                                                   ) 0
                                                      $ players boardWithTwoBanks
                                       }

boardWithTwoWinners :: Board
boardWithTwoWinners = boardWithOneWinner { players = Map.adjust (setPlayerCards [ Card $ CardValue Ace Clubs
                                                                                , Card $ CardValue Ace Hearts
                                                                                ]
                                                                ) 2
                                                   $ players boardWithOneWinner
                                         }
