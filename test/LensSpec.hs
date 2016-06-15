{-# LANGUAGE ScopedTypeVariables #-}

module LensSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Lens

spec = describe "lenses" $ do
  describe "_1" $ do
    it "works as a getter" $
      property $ \(a :: Int) (b :: Int) ->
        (a, b) ^. _1 == a
    it "works as a setter too" $
      property $ \(a :: Int) (a' :: Int) (b :: Int) ->
        let (x, b) = (a, b) & _1 .~ a'
        in x == a'
