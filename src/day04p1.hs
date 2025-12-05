-----------------------------------------------------------------------------
-- | Day 4 - Part 1
-- 
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.Set as S (Set, fromList)
-----------------------------------------------------------------------------
-- Data
-----------------------------------------------------------------------------

-- | Main
main = do
  args <- getArgs
  input <- readFile . head $ args
  let spaceList = getRolls 0 0 . Prelude.map (filter (/= '\r')) . lines $ input
  putStrLn . show . length . filter (accessible (fromList spaceList)) $ spaceList

getRolls :: Int -> Int -> [String] -> [(Int,Int)]
getRolls _ _ [] = []
getRolls y x ([]:rs)
  = getRolls (y + 1) 0 rs
getRolls y x (('@':es):rs) = (x,y):(getRolls y (x + 1) (es:rs))
getRolls y x (('.':es):rs) = (getRolls y (x + 1) (es:rs))

accessible :: Set (Int, Int) -> (Int,Int) -> Bool
accessible spaceSet (x,y)
  = let neighbors = [(x - 1, y - 1), (x, y - 1), (x + 1, y - 1), (x - 1, y), (x + 1, y), (x - 1, y + 1),  (x, y + 1), (x + 1, y + 1)]
    in (<4) . length . filter (==True) . map (\x -> elem x spaceSet) $ neighbors