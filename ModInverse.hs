-- (A, log O, log R) = (17, 5, 4)

import Control.Monad
import System.IO

import Util

egcd :: Int -> Int -> (Int, Int, Int)
egcd x y | x < y = (d, b, a)
  where (d, a, b) = egcd y x
egcd x y | y == 0 = (x, 1, 0)
egcd x y = (d, b, a - b*(div x y))
  where (d, a, b) = egcd y (mod x y)
        
-- p must be prime. If x is 0, we return 0.
modInverse :: Int -> Int -> Int
modInverse p 0 = 0
modInverse p x = if inv > 0 then mod inv p else p - mod (-inv) p
  where (_, _, inv) = egcd p x

main :: IO ()
main = do
  putStrLn "00000000"
  hFlush stdout
  forever $ do
    line <- getLine
    let action = binToInt line
        inverse = modInverse 17 action
        obs = intToBin 5 inverse
        reward = intToBin 4 (abs (10 - inverse))
    putStrLn (obs ++ reward)
    hFlush stdout