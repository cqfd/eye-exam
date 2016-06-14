{-# LANGUAGE ScopedTypeVariables #-}

import Test.Hspec
import Test.QuickCheck

import Control.Lens

main :: IO ()
main = hspec $ do

  describe "lenses" $ do
    describe "_1" $ do
      it "works as a getter" $
        property $ \(a :: Int) (b :: Int) ->
          (a, b) ^. _1 == a
      it "works as a setter too" $
        property $ \(a :: Int) (a' :: Int) (b :: Int) ->
          let (x, b) = (a, b) & _1 .~ a'
          in x == a'

  describe "prisms" $ do
    describe "_Right" $ do
      it "might work as a getter" $
        property $ \(a :: Int) ->
          (Right a) ^?! _Right == a
      it "might not though" $
        property $ \(a :: Int) ->
          (Left a :: Either Int String) ^? _Right == Nothing
      it "might work as a setter too" $
        property $ \(a :: Int) (a' :: Int) ->
          let new = (Right a) & _Right .~ a'
          in new ^?! _Right == a'
      it "might not though" $
        property $ \(a :: Int) (a' :: Int) ->
          let old = Right a
              new = old & _Left .~ a'
          in new == old

  describe "traversals" $ do
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
