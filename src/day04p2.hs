-----------------------------------------------------------------------------
-- | Day 4 - Part 2
-- 
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.Set as S (Set, fromList, filter, difference)

-----------------------------------------------------------------------------
-- Data
-----------------------------------------------------------------------------

-- | Main
main = do
  args <- getArgs
  input <- readFile . head $ args
  let space = fromList . getRolls 0 0 . Prelude.map (Prelude.filter (/= '\r')) . lines $ input
  putStrLn . show . length . difference space . recAccesibles $ space

getRolls :: Int -> Int -> [String] -> [(Int,Int)]
getRolls _ _ [] = []
getRolls y x ([]:rs)
  = getRolls (y + 1) 0 rs
getRolls y x (('@':es):rs) = (x,y):(getRolls y (x + 1) (es:rs))
getRolls y x (('.':es):rs) = (getRolls y (x + 1) (es:rs))

accessible :: Set (Int, Int) -> (Int,Int) -> Bool
accessible space (x,y)
  = let neighbors = [(x - 1, y - 1), (x, y - 1), (x + 1, y - 1), (x - 1, y), (x + 1, y), (x - 1, y + 1),  (x, y + 1), (x + 1, y + 1)]
    in (<4) . length . Prelude.filter (==True) . map (\x -> elem x space) $ neighbors

recAccesibles :: Set (Int,Int) -> Set (Int,Int)
recAccesibles space
  = let accessibles = S.filter (accessible space) space
        newSpace = difference space accessibles
    in if (length newSpace < length space) then
         recAccesibles newSpace 
       else 
         space