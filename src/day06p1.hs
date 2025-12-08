-----------------------------------------------------------------------------
-- | Day 1 - Part 1
-- 
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.List (transpose)

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

-- | Main
main = do
  args <- getArgs
  input <- readFile . head $ args
  let operations = map words . lines $ input
  putStrLn . show . foldl1 (+) . map (operate . reverse) . transpose $ operations

-- | Apply multiplication or addition
operate ("*":ns)
  = foldl1 (*) (map read ns)
operate ("+":ns)
  = foldl1 (+) (map read ns)
