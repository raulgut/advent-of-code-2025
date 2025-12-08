-----------------------------------------------------------------------------
-- | Day 1 - Part 2
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
  let operations = transpose . Prelude.map (Prelude.filter (/= '\r')) . lines $ input
  putStrLn . show . foldl1 (+) . operate $ operations

-- | Apply multiplication or addition
operate :: [String] -> [Int]
operate [] = []
operate (n1:ns)
  = let (n1',op) = (init n1, last n1)
        (ns1,nss) = span (not . allBlanks) ns 
    in (case op of 
          '*' -> foldl1 (*) (map read (n1':ns1))
          '+' -> foldl1 (+) (map read (n1':ns1))):(if (not . null $ nss) then (operate . tail $ nss) else [])

allBlanks = and . map (==' ')