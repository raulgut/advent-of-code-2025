-----------------------------------------------------------------------------
-- | Day 3 - Part 2
-- 
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.Char (digitToInt)

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

-- | Main
main = do
  args <- getArgs
  input <- readFile . head $ args
  let banks = (map (map digitToInt . filter (/= '\r')) . lines $ input) :: [[Int]]
  putStrLn . show . foldl1 (+) . map (read . foldl1 (++) . map show) . map (getHighers [] [0 | _ <- [0..11]]) $ banks

-- | process the bank and get the highers
getHighers :: [Int] -> [Int] -> [Int] -> [Int]
getHighers [] xs [] = xs
getHighers [] (x:xs) (y:ys) 
  | length (x:xs) > length (y:ys)
      = let (fixed,variable) = splitAt (length (x:xs) - length (y:ys)) (x:xs) 
        in getHighers fixed variable (y:ys)
getHighers prev (x:xs) (y:ys) 
      = if (x >= y) then
          getHighers (prev ++ [x]) xs  (y:ys)
        else
          getHighers [] (prev ++ (y:[0 | _ <- xs])) ys 
getHighers prev [] (y:ys) = getHighers [] prev ys