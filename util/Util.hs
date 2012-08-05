module Util (binToInt, intToBin) where

charToBit :: Char -> Int
charToBit x = case x of
  '0' -> 0
  '1' -> 1

binToInt :: String -> Int
binToInt xs = sum $ zipWith (*) (map charToBit xs)
              (reverse [2^(k-1) | k <- [1..length xs]])

intToBin :: Int -> Int -> String
intToBin 0 _ = ""
intToBin len n = case mod n 2 of
  0 -> intToBin (len-1) (div n 2) ++ "0"
  1 -> intToBin (len-1) (div n 2) ++ "1"