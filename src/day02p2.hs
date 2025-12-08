-----------------------------------------------------------------------------
-- | Day 2 - Part 2
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
    in or [isInvalidSize size v | size <- [1..(div vlen 2)], (mod vlen size) == 0]

-- | check if it is an invalid string
isInvalidSize size v 
  = let (p:ps) = splitAtRec size v
    in and . map (== p) $ ps

-- | obtain all the possibilities given a size
splitAtRec :: Int -> [a] -> [[a]]
splitAtRec size [] = []
splitAtRec size ls 
  = if (size > length ls) then
      [ls] 
    else 
      let (l,ls') = splitAt size ls 
      in [l] ++ (splitAtRec size ls')     
