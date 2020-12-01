module AoC.Days.Day00Spec
  ( spec,
  )
where

import AoC.Prelude
import Test.Hspec

spec :: Spec
spec = describe "Day00" $ do
  it "begins... :)" $ do
    (42 :: Int) `shouldBe` (42 :: Int)
