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
dayTester (Left s) _ _ _ = it "is an invalid day" $ expectationFailure s
dayTester (Right day) parse (solveA, solA) (solveB, solB) = do
  beforeAll (loadInputFile day) $ do
    describe (displayDay day) $ do
      it "parse the input" $ \i -> do
        parse i `shouldSatisfy` isJust
      it "solve part one" $ \i -> do
        solveA <$> parse i `shouldBe` Just solA
      it "solve part two" $ \i -> do
        solveB <$> parse i `shouldBe` Just solB

dayTesterPending ::
  forall a b c.
  (Show a, Show b, Show c, Eq b, Eq c) =>
  Either String Day ->
  ParseTest a ->
  RunTest a b ->
  RunTest a c ->
  Spec
dayTesterPending (Left s) _ _ _ = it "is an invalid day" $ expectationFailure s
dayTesterPending (Right day) p sa sb = do
  beforeAll (loadInputFile day) $ do
    describe (displayDay day) $ do
      it "parse the input" $ \i -> case p of
        ParsePending -> pending
        Parse parse -> parse i `shouldSatisfy` isJust
      it "solve part one" $ \i -> case sa of
        RunPending -> pending
        Run parse (solveA, solA) -> solveA <$> parse i `shouldBe` Just solA
      it "solve part two" $ \i -> case sb of
        RunPending -> pending
        Run parse (solveB, solB) -> solveB <$> parse i `shouldBe` Just solB

parsePending :: ParseTest ()
parsePending = ParsePending

runPending :: RunTest a ()
runPending = RunPending

data ParseTest a
  = ParsePending
  | Parse (String -> Maybe a)

data RunTest a b
  = RunPending
  | Run (String -> Maybe a) (a -> b, b)
