-----------------------------------------------------------------------------
-- | Day 7 - Part 1
-- 
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.List (sort)
import Control.Monad.State (State (..), get, put, execState)
import Data.Set as S (Set, fromList, filter)
import Data.Maybe (isJust, fromJust)
import Data.Graph(buildG, Graph, reachable)
import Data.Map as M (fromList, (!))

-----------------------------------------------------------------------------
-- Data
-----------------------------------------------------------------------------

-- | Current State
data TMDiagram
  = TMDiagram { maxRows :: Int 
              , maxColumns :: Int
              , startingPoing :: Maybe (Int,Int)
              , splitters :: [(Int,Int)]
              , splitterArcs :: [((Int,Int),(Int,Int))]
              } deriving Show

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

-- | Main
main = do
  args <- getArgs
  input <- readFile . head $ args
  let rows = Prelude.map (Prelude.filter (/= '\r')) . lines $ input
  let diagram = computeSplittersArcs . (\rs -> execState (processRows 0 0 rs) (TMDiagram (length rows) (length . head $ rows) Nothing [] [])) $ rows
  let (x,y) = fromJust . startingPoing $ diagram
  let mapping = M.fromList . zip ((x,y):splitters diagram) $ [0..(length . splitters $ diagram)]
  let graph = buildG (0, length (splitters diagram)) (map (\(x,y) -> (mapping!x,mapping!y)) . splitterArcs $ diagram)
  putStrLn . show . (\n -> n-1) . length $ reachable graph (mapping!(x,y))

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
        initArc = fromJust . getMinimum . S.filter (\(x',y') -> y' > y) . S.filter (\(x',y') -> x' == x) $ splitterSet
    in diagram { splitterArcs = ((x,y),initArc):(concatMap (computeSplitterArcs splitterSet) splitterList) }
computeSplitterArcs :: Set (Int,Int) -> (Int,Int) -> [((Int,Int),(Int,Int))]
computeSplitterArcs splitterSet (x,y)
  = let arcs = [getMinimum . S.filter (\(x',y') -> y' > y) . S.filter (\(x',y') -> x' == x - 1) $ splitterSet, getMinimum . S.filter (\(x',y') -> y' > y) . S.filter (\(x',y') -> x' == x + 1) $ splitterSet]
    in map ((\ar -> ((x,y),ar)) . fromJust) . Prelude.filter isJust $ arcs

-- | Auxiliary function
getMinimum :: (Ord a) => Set a -> Maybe a
getMinimum ss = if (not . null $ ss) then (Just . minimum $ ss) else Nothing
