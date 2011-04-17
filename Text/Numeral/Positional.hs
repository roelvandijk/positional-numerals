{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , ScopedTypeVariables
           , UnicodeSyntax     
           #-}

module Text.Numeral.Positional where
    -- ( toPositional
    -- , digitSymbol

    --   -- *Positional numeral systems
    -- , binary
    -- , negabinary
    -- , ternary
    -- , octal
    -- , decimal
    -- , negadecimal
    -- , hexadecimal
    -- ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- base
import Control.Monad         ( (>>), fmap, mapM )
import Data.Eq               ( (==) )
import Data.Bool             ( Bool(True), otherwise )
import Data.Function         ( ($) )
import Data.List             ( foldl', genericReplicate, genericIndex
                             , map, reverse, zipWith, iterate
                             )
import Data.Maybe            ( Maybe(Nothing, Just) )
import Data.Monoid           ( Monoid, mconcat )
import Data.Ord              ( (<), (>) )
import Data.String           ( IsString, fromString )
import Prelude               ( (+), (-), Integral, fromIntegral
                             , Integer, fromInteger, toInteger
                             , negate, abs, error, quotRem, signum
                             , Int
                             , RealFrac, floor
                             )

-- base-unicode-symbols
import Data.Bool.Unicode     ( (∧) )
import Data.Eq.Unicode       ( (≡), (≢) )
import Data.Function.Unicode ( (∘) )
import Data.Monoid.Unicode   ( (⊕) )
import Data.Ord.Unicode      ( (≥) )
import Prelude.Unicode       ( (⋅), (÷) )


-------------------------------------------------------------------------------

intToDigits ∷ Integer → Integer → [Integer]
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

        quotRem' ∷ Integral α ⇒ α → α → (α, α)
        quotRem' x d = let qr@(q, r) = x `quotRem` d
                       in if r < 0
                          then (q + 1, r - d)
                          else qr

intFromDigits ∷ Integer → [Integer] → Integer
intFromDigits b = sum' ∘ zipWith (⋅) (iterate (⋅ b) 1)
    where sum' = foldl' (+) 0

fracToDigits ∷ ∀ α β. (RealFrac α, Integral β) ⇒ Integer → Int → α → [β]
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

fracFromDigits ∷ ∀ α β. (RealFrac α, Integral β) ⇒ Integer → [β] → α
fracFromDigits base = go 1
    where
      go ∷ α → [β] → α
      go x []     = 0
      go x (d:ds) = let x' = x ÷ fromInteger base
                        d' = fromIntegral d
                    in d' ⋅ x' + go x' ds

prop_polynomial ∷ Integer → Integer → Bool
prop_polynomial b n | b ≡ 0    ∧ n ≢ 0     = True
                    | b ≡ (-1) ∧ abs n > 1 = True
                    | otherwise            = n ≡ (intFromDigits b $ intToDigits b n)

-------------------------------------------------------------------------------

toPositional ∷ (IsString s, Monoid s) 
             ⇒ (Integer → s) → Integer → Integer → Maybe s
toPositional f b n | b ≡ 0         = Nothing
                   | n < 0 ∧ b > 0 = fmap ("-" ⊕) $ repr (abs n)
                   | otherwise     = repr n
    where repr x = fmap mconcat ∘ mapM f' ∘ reverse ∘ intToDigits b $ x
          f' n | n ≥ abs b = Nothing
               | otherwise = Just $ f n

fromPositional ∷ Integer → [Integer] → Integer
fromPositional b = intFromDigits b ∘ reverse

-------------------------------------------------------------------------------

-- Digit symbols up to base 62.
-- TODO: array for O(1) lookup
digitSymbols ∷ IsString s ⇒ [s]
digitSymbols = map (\x → fromString [x]) $ ['0'..'9'] ⊕ ['A'..'Z'] ⊕ ['a'..'z']

digitSymbol ∷ IsString s ⇒ Integer → s
digitSymbol = (digitSymbols `genericIndex`)

-------------------------------------------------------------------------------

binary ∷ (Monoid s, IsString s) ⇒ Integer → Maybe s
binary = toPositional digitSymbol 2

negabinary ∷ (Monoid s, IsString s) ⇒ Integer → Maybe s
negabinary = toPositional digitSymbol (-2)

ternary ∷ (Monoid s, IsString s) ⇒ Integer → Maybe s
ternary = toPositional digitSymbol 3

octal ∷ (Monoid s, IsString s) ⇒ Integer → Maybe s
octal = toPositional digitSymbol 8

decimal ∷ (Monoid s, IsString s) ⇒ Integer → Maybe s
decimal = toPositional digitSymbol 10

negadecimal ∷ (Monoid s, IsString s) ⇒ Integer → Maybe s
negadecimal = toPositional digitSymbol (-10)

hexadecimal ∷(Monoid s, IsString s) ⇒ Integer → Maybe s
hexadecimal = toPositional digitSymbol 16
