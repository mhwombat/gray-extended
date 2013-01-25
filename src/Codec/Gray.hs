-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.Gray
-- Copyright   :  (c) Amy de Buitléir 2011-2012
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Gray encoding schemes. A Gray code is a list of values such that two
-- successive values differ in only one digit. Usually the term /Gray code/
-- refers to the Binary Reflected Gray code (BRGC), but non-binary Gray codes
-- have also been discovered. Some Gray codes are also /cyclic/: the last and
-- first values differ in only one digit.
--
-----------------------------------------------------------------------------
{-# LANGUAGE UnicodeSyntax #-}

module Codec.Gray 
  (
    grayCodes,
    naryGrayCodes
  ) where

import Data.List (foldl')

-- | @'grayCodes' k@ generates the list of Binary Reflected Gray Code (BRGC)
--   numbers of length k. This code is cyclic.
grayCodes ∷ Int → [[Bool]]
grayCodes 0 = [[]]
grayCodes k = 
  let xs = grayCodes (k-1) in map (False:) xs ++ map (True:) (reverse xs)

-- | @'naryGrayCodes' xs k@ generates a non-Boolean (or n-ary) Gray code of
--   length @k@ using the elements of @x@ as "digits". This code is cyclic.
--
--   Ex: @'naryGrayCodes' "012" 4@ generates a ternary Gray code that is 
--   four digits long.
naryGrayCodes ∷ [a] → Int → [[a]]
naryGrayCodes xs 1 = map (\x → [x]) xs
naryGrayCodes xs k = snd $ foldl' prefixAndShift (ys,[]) xs'
  where ys = naryGrayCodes xs 1
        xs' = naryGrayCodes xs (k-1)

-- | Shift elements right.
shift ∷ [a] → [a]
shift as = last as : init as

prefixAndShift ∷ ([[a]],[[a]]) → [a] → ([[a]],[[a]])
prefixAndShift (ys,zs) xs = (shift ys, zs ++ (map (xs++) ys))

