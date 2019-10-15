import Test.Hspec

import           Control.Lens ((^.), (.~), (%~))
import qualified Data.Map as Map
import qualified Data.Set as Set

import Board        ( Bank(..), Board, Player, banks, isInGame, onBoardCards
                    , playerBet, playerCards, playerMoney, players)
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
      fillBanks boardWithLeftPlayer    `shouldBe` (players %~ Map.adjust (isInGame .~ False) 1) boardWithTwoBanks

  describe "BoardUtils.giveMoney (no winner)" $ do
    it "no banks" $
      giveMoney boardWithEqualHandValues `shouldBe` boardWithEqualHandValues
    it "one bank" $
      giveMoney boardWithOneBank         `shouldBe` boardWithEqualHandValues
    it "two banks" $
      giveMoney boardWithTwoBanks        `shouldBe` boardWithEqualHandValues

  describe "BoardUtils.giveMoney (with winner)" $ do
    it "one winner" $
      giveMoney boardWithOneWinner  `shouldBe` ( players %~ Map.adjust (playerMoney .~ 1400) 0
                                               $ banks   .~ [initialBank]
                                               $ boardWithOneWinner
                                               )

    it "two winners" $
      giveMoney boardWithTwoWinners `shouldBe` ( players %~ (Map.adjust (playerMoney .~ 1000) 0)
                                                          . (Map.adjust (playerMoney .~ 1300) 2)
                                               $ banks   .~ [initialBank]
                                               $ boardWithTwoWinners
                                               )

initialBank :: Bank
initialBank = Bank { _bankMoney        = 0
                   , _bankParticipants = Set.fromList [0, 1, 2, 3]
                   }

defaultBoard :: Board
defaultBoard = players %~ Map.map ((playerBet .~ 0) . (playerMoney .~ 1000)) $ board
  where
    board :: Board
    board = createBoardUsingGen 0 1 4

boardWithEqualHandValues :: Board
boardWithEqualHandValues = onBoardCards .~ [ Card $ CardValue Ace   Spades
                                           , Card $ CardValue King  Spades
                                           , Card $ CardValue Jack  Spades
                                           , Card $ CardValue Queen Spades
                                           , Card $ CardValue Ten   Spades
                                           ]
                         $ defaultBoard

boardWithBets :: Board
boardWithBets = players %~ Map.map (addBet 100) $ boardWithEqualHandValues

boardWithDifferentBets :: Board
boardWithDifferentBets = players %~ (Map.adjust (addBet 100) 0) . (Map.adjust (addBet 100) 1) $ boardWithBets

boardWithLeftPlayer :: Board
boardWithLeftPlayer = players %~ Map.adjust (isInGame .~ False) 1 $ boardWithDifferentBets

boardWithOneBank :: Board
boardWithOneBank = banks   .~ [bank]
                 $ players %~ Map.map (playerMoney .~ 900)
                 $ boardWithEqualHandValues
  where
    bank :: Bank
    bank = Bank { _bankMoney        = 400
                , _bankParticipants = Map.keysSet $ boardWithEqualHandValues^.players
                }

boardWithTwoBanks :: Board
boardWithTwoBanks = banks   %~ (bank :)
                  $ players %~ (Map.adjust (playerMoney .~ 800) 0) . (Map.adjust (playerMoney .~ 800) 1)
                  $ boardWithOneBank
  where
    bank :: Bank
    bank = Bank { _bankMoney        = 200
                , _bankParticipants = Set.fromList [0, 1]
                }

boardWithOneWinner :: Board
boardWithOneWinner = onBoardCards .~ [ Card $ CardValue Ace   Spades
                                     , Card $ CardValue King  Spades
                                     , Card $ CardValue Ace   Diamonds
                                     , Card $ CardValue King  Diamonds
                                     , Card $ CardValue Two   Spades
                                     ]
                   $ players      %~ Map.adjust (playerCards .~ [ Card $ CardValue King Clubs
                                                                , Card $ CardValue King Hearts
                                                                ]
                                                ) 0
                   $ boardWithTwoBanks

boardWithTwoWinners :: Board
boardWithTwoWinners = players %~ Map.adjust (playerCards .~ [ Card $ CardValue Ace Clubs
                                                            , Card $ CardValue Ace Hearts
                                                            ]
                                            ) 2
                    $ boardWithOneWinner
