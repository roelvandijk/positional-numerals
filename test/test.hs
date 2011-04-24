{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}

module Main where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.Bool     ( Bool(True), not )
import Data.Ord      ( (>) )
import Data.Function ( ($) )
import Prelude       ( Integer, abs )
import System.IO     ( IO )

-- from base-unicode-symbols:
import Data.Eq.Unicode   ( (≡), (≢) )
import Data.Bool.Unicode ( (∧) )

-- from test-framework:
import Test.Framework ( Test, defaultMain, testGroup )

-- from test-framework-quickcheck2:
import Test.Framework.Providers.QuickCheck2 ( testProperty )

-- from QuickCheck:
import Test.QuickCheck.Property ( Property, (==>) )

-- from positional-numerals:
import Text.Numeral.Positional
import Text.Numeral.Positional.Digits


--------------------------------------------------------------------------------
-- Test suite
--------------------------------------------------------------------------------

main ∷ IO ()
main = defaultMain tests

tests ∷ [Test]
tests = [ testProperty "zoei" prop_polynomial ]

prop_polynomial ∷ Integer → Integer → Property
prop_polynomial b n = pre ==> n ≡ (intFromDigits b $ intToDigits b n)
    where
      pre = not (b ≡ 0 ∧ n ≢ 0)
          ∧ not (b ≡ -1 ∧ abs n > 1)
