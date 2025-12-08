-----------------------------------------------------------------------------
-- | Day 7 - Part 2
-- 
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.List (sort)
import Control.Monad.State (State (..), get, put, execState)
import Data.Set as S (Set, fromList, filter)
import Data.Maybe (isJust, fromJust)
import Data.Graph(buildG, Graph, reachable)
import Data.Map as M (Map, fromList, (!), empty, lookup, insert)

-----------------------------------------------------------------------------
-- Data
-----------------------------------------------------------------------------

-- | Current State
data TMDiagram
  = TMDiagram { maxRows :: Int 
              , maxColumns :: Int
              , startingPoing :: Maybe (Int,Int)
              , splitters :: [(Int,Int)]
              , splitterArcs :: Map (Int,Int) [(Int,Int)]
              } deriving Show

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

-- | Main
main = do
  args <- getArgs
  input <- readFile . head $ args
  let rows = Prelude.map (Prelude.filter (/= '\r')) . lines $ input
  let diagram = computeSplittersArcs . (\rs -> execState (processRows 0 0 rs) (TMDiagram (length rows) (length . head $ rows) Nothing [] M.empty)) $ rows
  let (x,y) = fromJust . startingPoing $ diagram
  let firstNeighbor = fromJust . getMinimum . S.filter (\(x',y') -> y' > y) . S.filter (\(x',y') -> x' == x) . S.fromList . splitters $ diagram
  putStrLn . show . M.lookup firstNeighbor . processPaths (splitterArcs diagram) M.empty . splitters $ diagram

-- | Process tachyon manifolds diagram
processRows :: Int -> Int -> [String] -> State TMDiagram ()
processRows x y [] = return ()
processRows x y ([]:ys) = processRows 0 (y + 1) ys
processRows x y (('.':xs):ys) = processRows (x + 1) y (xs:ys)  
processRows x y (('S':xs):ys) = do tmdiag <- get
                                   put (tmdiag { startingPoing = Just (x,y)})
                                   processRows (x + 1) y (xs:ys)
processRows x y (('^':xs):ys) = do tmdiag <- get
                                   put (tmdiag { splitters = (x,y):(splitters tmdiag)})
                                   processRows (x + 1) y (xs:ys)

-- | Obtain the arcs of the graph
computeSplittersArcs :: TMDiagram -> TMDiagram
computeSplittersArcs diagram
  = let splitterList = splitters diagram 
        splitterSet = S.fromList splitterList
        (x,y) = fromJust . startingPoing $ diagram
    in diagram { splitterArcs = M.fromList $ (map (computeSplitterArcs splitterSet) splitterList) }
computeSplitterArcs :: Set (Int,Int) -> (Int,Int) -> ((Int,Int),[(Int,Int)])
computeSplitterArcs splitterSet (x,y)
  = let arcs = [getMinimum . S.filter (\(x',y') -> y' > y) . S.filter (\(x',y') -> x' == x - 1) $ splitterSet, getMinimum . S.filter (\(x',y') -> y' > y) . S.filter (\(x',y') -> x' == x + 1) $ splitterSet]
    in ((x,y),map fromJust . Prelude.filter isJust $ arcs)

-- | Auxiliary function
getMinimum :: (Ord a) => Set a -> Maybe a
getMinimum ss = if (not . null $ ss) then (Just . minimum $ ss) else Nothing

-- | From bottom to top, get possible paths
processPaths :: Map (Int,Int) [(Int,Int)] -> Map (Int,Int) Int -> [(Int,Int)] -> Map (Int,Int) Int 
processPaths arcs acc [] = acc
processPaths arcs acc (s:ss)
  = let (Just neighbors) = M.lookup s arcs
    in if null neighbors then
         processPaths arcs (M.insert s 2 acc) ss 
       else 
         processPaths arcs ((\v -> M.insert s v acc) . (\v -> if length neighbors == 1 then v + 1 else v) . foldl1 (+) . map (\n -> fromJust . M.lookup n $ acc) $ neighbors) ss