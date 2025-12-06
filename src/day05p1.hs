-----------------------------------------------------------------------------
-- | Day 5 - Part 1
-- 
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec (Parser, many, try)
import Text.Parsec.Char (digit, string, char)
import Text.Parsec.Combinator (many1, eof)
import Text.Parsec.Prim (parse, (<|>), (<?>))

-----------------------------------------------------------------------------
-- Data
-----------------------------------------------------------------------------

-- | Main
main = do
  args <- getArgs
  input <- readFile . head $ args
  let (freshIngredients,ingredients) = (extractDataBaseIngredients input) :: ([(Int,Int)],[Int]) 
  putStrLn . show . length . filter (\x -> isFreshList x freshIngredients) $ ingredients

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