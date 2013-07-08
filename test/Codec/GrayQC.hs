module Codec.GrayQC
  (
    test
  ) where

import Codec.Gray
import Data.List (nub)
import Test.Framework as TF (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck ((==>), Property, Gen, Arbitrary, arbitrary, choose, 
  property, sized)

prop_successive_values_differ_in_one_place1 :: Int -> Property
prop_successive_values_differ_in_one_place1 k = k > 0 ==>
  nub (diffCounts xs) == [1]
    where xs = grayCodes k'
          k' = min 10 k -- avoid long tests

data TestParms = TestParms String Int deriving Show

sizedTestParms :: Int -> Gen TestParms
sizedTestParms n = do
  let n' = 2 + (min 8 n) -- keep number of digits and bits low to speed up tests
  let digits = take n' ['a' .. 'z']
  k <- choose (1,min 4 (n+1))
  return $ TestParms digits k

instance Arbitrary TestParms where
  arbitrary = sized sizedTestParms
  
prop_successive_values_differ_in_one_place2 :: TestParms -> Property
prop_successive_values_differ_in_one_place2 (TestParms ds k) = property $
  nub (diffCounts xs) == [1]
    where xs = naryGrayCodes ds k

diffCounts :: Eq b => [[b]] -> [Int]
diffCounts [] = []
diffCounts [_] = []
diffCounts (x1:x2:xs) = (diffCount x1 x2) : diffCounts (x2:xs)

diffCount :: Eq b => [b] -> [b] -> Int
diffCount as bs = length $ filter (\x -> x) $ zipWith (/=) as bs

test :: Test
test = testGroup "Codec.GrayQC"
  [
    testProperty "prop_successive_values_differ_in_one_place1"
      prop_successive_values_differ_in_one_place1,
    testProperty "prop_successive_values_differ_in_one_place2"
      prop_successive_values_differ_in_one_place2
  ]


