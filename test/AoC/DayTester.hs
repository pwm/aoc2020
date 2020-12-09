module AoC.DayTester where

import AoC.Lib.Day
import AoC.Lib.File
import AoC.Prelude
import Test.Hspec

dayTester ::
  (Show a, Show b, Show c, Eq b, Eq c) =>
  Either String Day ->
  (String -> Maybe a) ->
  (a -> b, b) ->
  (a -> c, c) ->
  Spec
dayTester (Right day) parse (solveA, solA) (solveB, solB) = do
  beforeAll (loadInputFile day) $ do
    describe (displayDay day) $ do
      it "parse the input" $ \i -> do
        parse i `shouldSatisfy` isJust
      it "solve part one" $ \i -> do
        solveA <$> parse i `shouldBe` Just solA
      it "solve part two" $ \i -> do
        solveB <$> parse i `shouldBe` Just solB
dayTester (Left s) _ _ _ = it "is an invalid day" $ expectationFailure s