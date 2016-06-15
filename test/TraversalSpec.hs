{-# LANGUAGE ScopedTypeVariables #-}

module TraversalSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Lens

spec = describe "traversals" $ do
  describe "both" $ do
    it "lets you target both parts of a pair" $ do
      property $ \(a :: Int, b :: Int) ->
        (a, b) ^.. both == [a, b]
    it "composes with lenses and prisms" $ do
      property $ \(a :: Int, b :: String) ->
        (Left a, Right b) ^.. both._Left == [a]
  describe "traversed" $ do
    it "lets you get multiple things all at once" $
      [Left 1, Right "foo", Left 2, Right "bar", Left 3] ^.. traversed._Left `shouldBe` [1,2,3]
    it "lets you set multiple things all at once too" $
      let new = [Left 1, Right "foo", Left 2, Right "bar", Left 3] & traversed._Left %~ (+1) in
      new `shouldBe` [Left 2, Right "foo", Left 3, Right "bar", Left 4]
