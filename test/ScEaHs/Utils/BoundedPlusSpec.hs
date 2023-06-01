{-# LANGUAGE TypeApplications #-}

module ScEaHs.Utils.BoundedPlusSpec (spec) where

import Data.Proxy (Proxy (Proxy))
import ScEaHs.Utils.BoundedPlus (BoundedPlus (..))
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Testable (property),
    forAll,
    suchThat,
  )

newtype BoundedPlusTest = BoundedPlusTest Int deriving (Show, Eq)

instance BoundedPlus BoundedPlusTest where
  bound _ = 123

instance Arbitrary BoundedPlusTest where
  arbitrary = BoundedPlusTest <$> arbitrary

wrap' :: Int -> BoundedPlusTest
wrap' = wrap :: Int -> BoundedPlusTest

spec :: Spec
spec = describe "bounded plus" $ do
  let bound' = bound (Proxy @BoundedPlusTest)
  it "returns non-negative values values" $
    property ((\a b -> unwrap (wrap' a <+> b) >= 0) :: Int -> Int -> Bool)
  it "returns values less than 'bound'" $
    property ((\a b -> unwrap (wrap' a <+> b) < bound') :: Int -> Int -> Bool)
  it "bounded plus is equivalent to applying (`mod` bound) to the sum" $
    property ((\a b -> unwrap (wrap' a <+> b) == (a + b) `mod` bound') :: Int -> Int -> Bool)
