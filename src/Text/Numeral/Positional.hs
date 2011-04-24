{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , ScopedTypeVariables
           , UnicodeSyntax     
           #-}

module Text.Numeral.Positional where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Control.Monad         ( fmap, mapM )
import Data.Bool             ( otherwise )
import Data.Function         ( ($) )
import Data.List             ( genericIndex, map, reverse )
import Data.Maybe            ( Maybe(Nothing, Just) )
import Data.Monoid           ( Monoid, mconcat )
import Data.Ord              ( (<), (>) )
import Data.String           ( IsString, fromString )
import Prelude               ( Integer, abs )

-- from base-unicode-symbols:
import Data.Bool.Unicode     ( (∧) )
import Data.Eq.Unicode       ( (≡) )
import Data.Function.Unicode ( (∘) )
import Data.Monoid.Unicode   ( (⊕) )
import Data.Ord.Unicode      ( (≥) )

-- from positional-numerals:
import Text.Numeral.Positional.Digits ( Polynomial
                                      , intToDigits, intFromDigits 
                                      )

-------------------------------------------------------------------------------

-- | Calculate the representation of a value in a positional numeral
-- system.
toPositional ∷ (IsString s, Monoid s) 
             ⇒ (Integer → s) -- ^ Representation of digits.
             → Integer       -- ^ Base or Radix.
             → Integer       -- ^ Value to be converted.
             → Maybe s       -- ^ Representation of the value in a
                             -- positional numeral system.
toPositional f b n | b ≡ 0         = Nothing
                   | n < 0 ∧ b > 0 = fmap ("-" ⊕) $ repr (abs n)
                   | otherwise     = repr n
    where repr x = fmap mconcat ∘ mapM f' ∘ reverse ∘ intToDigits b $ x
          f' k | k ≥ abs b = Nothing
               | otherwise = Just $ f k

fromPositional ∷ Integer → Polynomial → Integer
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
