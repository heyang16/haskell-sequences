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
-- Returns '0' if input is not a character
toUpper :: Char -> Char
toUpper x
  | ord x >= 97 && ord x <= 122 = chr (ord x - 32)
  | ord x >= 65 && ord x <= 90 = x
  | otherwise = '0'


--
-- Sequences and series
--

-- Arithmetic sequence
-- Parameters: a is the first term, d is the common difference, n is the term number
-- Returns the nth term
arithmeticSeq :: Double -> Double -> Int -> Double
arithmeticSeq a d n
  = fromIntegral n * d + a

-- Geometric sequence
-- Parameters: a is the first term, r is the common ratio, n is the term number
-- Returns the nth term
geometricSeq :: Double -> Double -> Int -> Double
geometricSeq a r n
  = a * r ^ fromIntegral n

-- Arithmetic series
-- Parameters: a is the first term, d is the common difference, n is the term number
-- Returns the sum of the first n terms
arithmeticSeries :: Double -> Double -> Int -> Double
arithmeticSeries a d n
  = (fromIntegral n + 1) * (a + (d * fromIntegral n) / 2)

-- Geometric series
-- Parameters: a is the first term, r is the common ratio, n is the term number
-- Returns the sum of the first n terms
geometricSeries :: Double -> Double -> Int -> Double
geometricSeries a r n
  | r == 1 = a * (fromIntegral n + 1)
  | otherwise = a * ((1 - r ^ (fromIntegral n + 1)) / (1 - r))
