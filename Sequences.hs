module Sequences where

import Data.Char (ord, chr)

-- Returns the first argument if it is larger than the second,
-- the second argument otherwise
maxOf2 :: Int -> Int -> Int
maxOf2 x y
  = if x > y then x else y

-- Returns the largest of three Ints
maxOf3 :: Int -> Int -> Int -> Int
maxOf3 x y z = maxOf2 x (maxOf2 y z)

-- Returns True if the character represents a digit '0'..'9';
-- False otherwise
isADigit :: Char -> Bool
isADigit x = 48 <= ord x && ord x <= 57


-- Returns True if the character represents an alphabetic
-- character either in the range 'a'..'z' or in the range 'A'..'Z';
-- False otherwise
isAlpha :: Char -> Bool
isAlpha x 
  = (97 <= ord x && ord x <= 122) || (65 <= ord x && ord x <= 90)

-- Returns the integer [0..9] corresponding to the given character.
-- Note: this is a simpler version of digitToInt in module Data.Char,
-- which does not assume the precondition.
digitToInt :: Char -> Int
-- Pre: the character is one of '0'..'9'
digitToInt x
  = ord x - 48

-- Returns the upper case character corresponding to the input.
-- Uses guards by way of variety.
toUpper :: Char -> Char
toUpper x
  = chr (ord x - 32)

--
-- Sequences and series
--

-- Arithmetic sequence
arithmeticSeq :: Double -> Double -> Int -> Double
arithmeticSeq 
  = undefined

-- Geometric sequence
geometricSeq :: Double -> Double -> Int -> Double
geometricSeq 
  = undefined

-- Arithmetic series
arithmeticSeries :: Double -> Double -> Int -> Double
arithmeticSeries 
  = undefined

-- Geometric series
geometricSeries :: Double -> Double -> Int -> Double
geometricSeries 
  = undefined
