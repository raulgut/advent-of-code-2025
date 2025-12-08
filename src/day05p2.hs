-----------------------------------------------------------------------------
-- | Day 5 - Part 2
-- 
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec (Parser, many, try)
import Text.Parsec.Char (digit, string, char)
import Text.Parsec.Combinator (many1, eof)
import Text.Parsec.Prim (parse, (<|>), (<?>))

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

-- | Main
main = do
  args <- getArgs
  input <- readFile . head $ args
  let (freshIngredients,_) = (extractDataBaseIngredients input) :: ([(Int,Int)],[Int]) 
  putStrLn . show . foldl1 (+) . map (\(x,y) -> y - x + 1) . processOverlaps $ freshIngredients

-- | Parses database
extractDataBaseIngredients :: String -> ([(Int,Int)],[Int])
extractDataBaseIngredients str
  = case (parse dbParser "" str) of
      Left err -> error . show $ err
      Right db -> db

-- | Extract fresh Ingredients and actual shelf
dbParser :: Parser ([(Int,Int)],[Int])
dbParser 
  = do fs <- many (freshIngredientsParser) 
       eol
       is <- many (ingredientsParser)
       return (fs,is)

-- | Fresh ingredients range
freshIngredientsParser :: Parser (Int,Int)
freshIngredientsParser 
  = do value1 <- natural
       char '-'
       value2 <- natural
       eol
       return $ (read value1,read value2)

-- | Actual shelf
ingredientsParser :: Parser Int
ingredientsParser 
  = do i <- natural
       eol
       return . read $ i

-- | natural number
natural :: Parser String
natural = many1 digit

-- | end of line
eol :: Parser String
eol = try (string "\n\r")
  <|> try (string "\r\n")
  <|> string "\n"
  <|> string "\r"
  <|> (do {eof ; return ""}) 
  <?> "end of line"

-- | Check if i is in one of the ranges
isFreshList :: Int -> [(Int,Int)] -> Bool
isFreshList i [] = False
isFreshList i (rg:rgs) = (isFresh i rg) || (isFreshList i rgs)

-- | Check if i is in the range
isFresh :: Int -> (Int,Int) -> Bool
isFresh i (l,r) = (i >= l) && (i <=r)

{-- | Inefficient way of counting 
getFreshIngredientsCount :: [(Int,Int)] -> Int -> [(Int,Int)] -> Int
getFreshIngredientsCount _ count [] = count
getFreshIngredientsCount processed count ((l,r):rgs)
  = let newrg = [i | i <- [l..r], not . isFreshList i $ processed]
    in getFreshIngredientsCount (processed ++ [(l,r)]) (count + length newrg) rgs
--}

-- | remove overlaps by compacting
processOverlaps :: [(Int,Int)] -> [(Int,Int)]
processOverlaps [] = []
processOverlaps [rg] = [rg]
processOverlaps ((l1,r1):rgs) 
  = let (nonOverlappingRange,restOfRanges) = processRangeOverlaps (l1,r1) [] rgs 
    in [nonOverlappingRange] ++ processOverlaps restOfRanges

-- | process range overlaps
processRangeOverlaps :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)] -> ((Int,Int),[(Int,Int)])
processRangeOverlaps rg processed [] = (rg,processed)
processRangeOverlaps (l1,r1) processed ((l2,r2):rgs)
  = if ((l1 <= l2) && (l2 <= r1) && (r1 <= r2)) then -- overlap  l1| l2|XXX r1| r2|
      processRangeOverlaps (l1,r2) [] (processed ++ rgs)
    else if ((l1 <= l2) && (l2 <= r1) && (r2 <= r1)) then -- overlap  l1| l2|XXX r2| r1|
      processRangeOverlaps (l1,r1) processed rgs
    else if ((l2 <= l1) && (l1 <= r2) && (r2 <= r1)) then -- overlap  l2| l1|XXX r2| r1|
      processRangeOverlaps (l2,r1) [] (processed ++ rgs)
    else if ((l2 <= l1) && (l1 <= r2) && (r1 <= r2)) then -- overlap  l2| l1|XXX r1| r2|
      processRangeOverlaps (l2,r2) processed rgs
    else
      processRangeOverlaps (l1,r1) (processed ++ [(l2,r2)]) rgs -- no overlap