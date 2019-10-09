import Test.Hspec

import Card
import CardUtils (handValueFromCardSet)

main :: IO ()
main = hspec $ do
  describe "CardUtils.handValueFromCardSet" $ do
    it "testing Kicker" $
      handValueFromCardSet [CardValue Queen Hearts
                          , CardValue Three Spades
                          , CardValue Ace Spades
                          , CardValue King Diamonds
                          , CardValue Five Spades
                          , CardValue Nine Clubs
                          , CardValue Eight Clubs] `shouldBe` Kicker Ace

    it "testing Pair" $
      handValueFromCardSet [CardValue Ace Hearts
                          , CardValue Three Spades
                          , CardValue Ace Spades
                          , CardValue King Diamonds
                          , CardValue Five Spades
                          , CardValue Nine Clubs
                          , CardValue Eight Clubs] `shouldBe` Pair Ace King

    it "testing TwoPairs" $
      handValueFromCardSet [CardValue Ten Spades
                          , CardValue King Diamonds
                          , CardValue King Spades
                          , CardValue Nine Clubs
                          , CardValue Ten Hearts
                          , CardValue Two Diamonds
                          , CardValue Two Clubs] `shouldBe` TwoPairs King Ten Nine

    it "testing ThreeOfAKind" $
      handValueFromCardSet [CardValue Ten Spades
                          , CardValue Ten Diamonds
                          , CardValue Five Diamonds
                          , CardValue Queen Spades
                          , CardValue Ten Clubs
                          , CardValue Jack Hearts
                          , CardValue Eight Clubs] `shouldBe` ThreeOfAKind Ten Queen

    it "testing Straight" $ do
      handValueFromCardSet [CardValue Ten Clubs
                          , CardValue Queen Hearts
                          , CardValue Eight Spades
                          , CardValue Nine Clubs
                          , CardValue Jack Diamonds
                          , CardValue Seven Hearts
                          , CardValue Two Spades] `shouldBe` Straight Queen
      handValueFromCardSet [CardValue Two Clubs
                          , CardValue Five Clubs
                          , CardValue Ten Spades
                          , CardValue Four Hearts
                          , CardValue Three Spades
                          , CardValue Four Diamonds
                          , CardValue Ace Diamonds] `shouldBe` Straight Five

    it "testing Flush" $
      handValueFromCardSet [CardValue King Spades
                          , CardValue Six Spades
                          , CardValue Ace Spades
                          , CardValue Nine Diamonds
                          , CardValue Ten Spades
                          , CardValue Seven Spades
                          , CardValue Five Spades] `shouldBe` Flush Ace King Ten Seven Six

    it "testing FullHouse" $ do
      handValueFromCardSet [CardValue Ace Hearts
                          , CardValue Ace Clubs
                          , CardValue Three Spades
                          , CardValue Two Diamonds
                          , CardValue Two Clubs
                          , CardValue Four Spades
                          , CardValue Two Spades] `shouldBe` FullHouse Two Ace
      handValueFromCardSet [CardValue Ace Hearts
                          , CardValue Ace Clubs
                          , CardValue Three Spades
                          , CardValue Two Diamonds
                          , CardValue Two Clubs
                          , CardValue Ace Spades
                          , CardValue Two Spades] `shouldBe` FullHouse Ace Two

    it "testing Quads" $
      handValueFromCardSet [CardValue Two Spades
                          , CardValue Queen Spades
                          , CardValue Queen Clubs
                          , CardValue Two Hearts
                          , CardValue Queen Diamonds
                          , CardValue Three Clubs
                          , CardValue Queen Hearts] `shouldBe` Quads Queen Three

    it "testing StraightFlush" $
      handValueFromCardSet [CardValue Ten Clubs
                          , CardValue Jack Spades
                          , CardValue Six Clubs
                          , CardValue Seven Clubs
                          , CardValue Nine Clubs
                          , CardValue King Diamonds
                          , CardValue Eight Clubs] `shouldBe` StraightFlush Ten

    it "testing RoyalFlush" $
      handValueFromCardSet [CardValue Ace Spades
                          , CardValue Three Clubs
                          , CardValue Ace Hearts
                          , CardValue King Spades
                          , CardValue Jack Spades
                          , CardValue Queen Spades
                          , CardValue Ten Spades] `shouldBe` RoyalFlush
