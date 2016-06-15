{-# LANGUAGE ScopedTypeVariables #-}

module PrismSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Lens

spec = describe "prisms" $ do
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
