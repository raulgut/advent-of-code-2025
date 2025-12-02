-----------------------------------------------------------------------------
-- | Day 1 - Part 2
-- 
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.List (sort)

-----------------------------------------------------------------------------
-- Data
-----------------------------------------------------------------------------

-- | Direction of the combination
data Direction 
  = R | L
  deriving Show             

-- | Instruction
data Instruction 
  = Instruction { direction :: Direction 
                , times :: Int 
                } deriving Show

-- | State
data State = State { position :: Int
                   , zeroVisits :: Int
                   }
-- | Main
main = do
  args <- getArgs
  input <- readFile . head $ args
  let instructions = map toInstruction . lines $ input
  putStrLn . show . process (State 50 0) $ instructions

-- | From String to Instruction (LX or RX)
toInstruction :: String -> Instruction
toInstruction ('L':xs) = Instruction L (read xs)
toInstruction ('R':xs) = Instruction R (read xs)
toInstruction str = error $ "Instruction " ++ str ++ " not allowed."

process :: State -> [Instruction] -> Int
process st [] = if (position st == 0) then (zeroVisits st) + 1 else zeroVisits st
process st ((Instruction L value):is)
  = let divTimes = (div value 100)
        newPosition = (position st) - (mod value 100)
        adjNewPosition = if (newPosition < 0) then (100 + newPosition) else newPosition
        newZeroVisits 
          = if (value /= 0) then
              if ((position st /= 0) && (newPosition < 0)) || (adjNewPosition == 0) then 
                (zeroVisits st) + divTimes + 1
              else
                (zeroVisits st) + divTimes
            else
              zeroVisits st
    in process (State adjNewPosition newZeroVisits) is
process st ((Instruction R value):is)
  = let divTimes = (div value 100)
        newPosition = (position st) + (mod value 100)
        adjNewPosition = if (newPosition > 99) then (newPosition - 100) else newPosition
        newZeroVisits 
          = if (value /= 0) then
              if (newPosition > 99) then 
                (zeroVisits st) + divTimes + 1
              else
                (zeroVisits st) + divTimes
            else
              zeroVisits st
    in process (State adjNewPosition newZeroVisits) is    