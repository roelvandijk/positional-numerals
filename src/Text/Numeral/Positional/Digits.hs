{-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables, UnicodeSyntax #-}

module Text.Numeral.Positional.Digits 
    ( Polynomial
    , intToDigits
    , intFromDigits
    , fracToDigits
    , fracFromDigits
    ) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Data.Bool             ( otherwise )
import Data.Function         ( ($) )
import Data.List             ( foldl', genericReplicate, zipWith, iterate )
import Data.Ord              ( (<), (>) )
import Prelude               ( (+), (-), Integral, fromIntegral
                             , Integer, fromInteger
                             , abs, error, quotRem, signum
                             , Int
                             , RealFrac, floor
                             )

-- from base-unicode-symbols:
import Data.Eq.Unicode       ( (≡) )
import Data.Function.Unicode ( (∘) )
import Data.Ord.Unicode      ( (≥) )
import Prelude.Unicode       ( (⋅), (÷) )

-------------------------------------------------------------------------------

-- | A polynomial represented by a list of coefficients. This
-- representation assumes that the base of the polynomial is known.
--
-- Assuming base 10:
--
-- > [8,6,7] = 8*10^0 + 6*10^1 + 7*10^2
type Polynomial = [Integer]

-- | Converts an integer to polynomial form.
-- 
-- >>> intToDigits 10 123 
-- [3,2,1] -- 3*10^0 + 2*10^1 + 1*10^2
--
-- >>> intToDigits (-8) 42
-- [2,3,1] -- 2*(-8)^0 + 3*(-8)^1 + 1*(-8)^2
--
-- Preconditions: 
-- 
-- * base &#x2261; 0 &#x2192; value &#x2261; 0
--
-- * base &#x2261; -1 &#x2192; value &#x2208; {-1..1}
--
-- Failure to meet the preconditions will result in runtime failure.
intToDigits ∷ Integer    -- ^ Base of the polynomial.
            → Integer    -- ^ Value to be converted.
            → Polynomial -- ^ Polynomial coefficients.
intToDigits b n 
    | n ≡ 0     = [0]
    | b ≡ 0     = error "intToDigits: base 0"
    | b ≡ (-1)  = case n of
                    (-1) → [0, 1]
                    1    → [1]
                    _    → error "intToDigits: base (-1)"
    | b ≡ 1     = genericReplicate (abs n) (signum n)
    | otherwise = intToDigits_b $ n
  where intToDigits_b 0 = []
        intToDigits_b x = let (q, r) = x `qr` b
                              in r : intToDigits_b q

        qr | b > 0     = quotRem
           | otherwise = quotRem'

        -- Alternative quotRem with a positive bias for the quotient.
        quotRem' ∷ Integral α ⇒ α → α → (α, α)
        quotRem' x d = let t@(q, r) = x `quotRem` d
                       in if r < 0
                          then (q + 1, r - d)
                          else t

-- | Converts a polynomial to an integer.
intFromDigits ∷ Integer    -- ^ Base of the polynomial.
              → Polynomial -- ^ Polynomial coefficients.
              → Integer    -- ^ Value of the polynomial.
intFromDigits b = sum' ∘ zipWith (⋅) (iterate (⋅ b) 1)
    where sum' = foldl' (+) 0

fracToDigits ∷ ∀ α β. (RealFrac α, Integral β) 
             ⇒ Integer -- ^ Numerical base.
             → Int     -- ^ Maximum number of digits in result.
             → α       -- ^ Value to be converted.
             → [β]     -- ^ Fractional digits.
fracToDigits base maxDigits value
    | value < 0 = error "fracToDigits: value < 0"
    | value ≥ 1 = error "fracToDigits: value >= 1"
    | base ≡ 0  = error "fracToDigits: base 0"
    | base ≡ 1  = error "fracToDigits: base 1"
    | otherwise = go maxDigits value
  where 
    go ∷ Int → α → [β]
    go 0 _ = []
    go _ 0 = []
    go n f = let x = f ⋅ fromIntegral base
                 y = floor x
             in y : go (n - 1) (x - fromIntegral y)

fracFromDigits ∷ ∀ α β. (RealFrac α, Integral β) 
               ⇒ Integer -- ^ Numerical base.
               → [β]     -- ^ Fractional digits.
               → α       -- ^ Resulting fractional value.
fracFromDigits base = go 1
    where
      go ∷ α → [β] → α
      go _ []     = 0
      go x (d:ds) = let x' = x ÷ fromInteger base
                        d' = fromIntegral d
                    in d' ⋅ x' + go x' ds
