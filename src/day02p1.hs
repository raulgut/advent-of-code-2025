-----------------------------------------------------------------------------
-- | Day 2 - Part 1
-- 
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.Text (splitOn, pack, unpack)

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

-- | Main
main = do
  args <- getArgs
  input <- readFile . head $ args
  let ranges = (map (map (read . unpack)) . map (splitOn (pack "-")) . splitOn (pack ",") . pack $ input) :: [[Int]]
  putStrLn . show . foldl1 (+) . concatMap getInvalidIDs $ ranges

-- | get invalid ids
getInvalidIDs :: [Int] -> [Int]
getInvalidIDs [a,b] = [v| v <- [a..b], isInvalid (show v)]

-- | check if the string is invalid (xx)
isInvalid :: String -> Bool
isInvalid v
  = let vlen = length v
    in if (mod vlen 2) == 0 then
         let (str1,str2) = splitAt (div vlen 2) v
         in (str1 == str2) 
       else
        False