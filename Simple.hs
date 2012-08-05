-- (A, log O, log R): (4, 1, 1)

-- A simple test - the output depends only on the previous action, and
-- there is one optimal action. The action 11 will give the percept
-- 11, and any other action will give 00.

import Control.Monad
import System.IO

f :: String -> String
f "11" = "11"
f _ = "00"

main :: IO ()
main = do
  putStrLn "00"
  hFlush stdout
  forever $ (getLine >>= putStrLn . f >> hFlush stdout)
