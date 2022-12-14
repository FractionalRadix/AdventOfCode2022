import Data.Char

main ::  IO()
main = do
  filecontent <- readFile "D:\\Haskell\\MyPrograms\\AoC_2022\\AoCDay05.txt"
  let inputLines = lines filecontent
  let stackLines = takeWhile containsCrate inputLines
  let instructions = tail $ dropWhile (/= []) inputLines
  let allStacks = map trimSpaces $ map (stackNumberN stackLines) [1..length(stackLines)+1]
  let parsedInstructions = map parseInstruction instructions

  let allStepsUsingCrateMover9000 = applyInstructions True allStacks parsedInstructions
  let answer1 = map head allStepsUsingCrateMover9000
  print $ "Answer 1: " ++ answer1    -- Answer is   "VWLCWGSDQ" (without double-quotes).
  let allStepsUsingCrateMover9001 = applyInstructions False allStacks parsedInstructions
  let answer2 = map head allStepsUsingCrateMover9001
  print $ "Answer 2: " ++ answer2  -- Answer is "TCGLQSLPW" (again, without double-quotes).


containsCrate :: String -> Bool
containsCrate str = elem '[' str

stackNumberN :: [String] -> Int -> String
stackNumberN stacks n = map (!!p) stacks
  where p = 4 * n - 3

-- Parse the instruction.
-- ALSO makes it go from 1-based to 0-based!
parseInstruction :: String -> (Int, Int, Int)
parseInstruction instr = (read $ tokens!!1, (read $ tokens!!3) - 1, (read $ tokens!!5) - 1)
  where tokens = split instr

-- This function adapted from https://stackoverflow.com/a/46595679/812149
split :: String -> [String]
split str = case break (==' ') str of
                (a, ' ':b) -> a : split b
                (a, "")    -> [a]

trimSpaces :: String -> String
trimSpaces str = dropWhile (==' ') str

-- Given a list of stacks, and two 0-based indices, split that list up at the indices.
-- This yields 5 possibly empty sublists: first part, singleton list at lowest index, middle part, singleton list at highest index, last part.
splitAtIndices :: Int -> Int -> [String] -> ([String],String,[String],String,[String])
splitAtIndices n1 n2 list = (firstPart, list!!lowestIdx, middlePart, list!!highestIdx, lastPart)
 where lowestIdx = min n1 n2
       highestIdx = max n1 n2
       firstPart = take (lowestIdx) list 
       middlePart = take (highestIdx - lowestIdx - 1) $ drop (lowestIdx + 1) $ list
       lastPart = drop (highestIdx + 1) list


-- Note that the instructions here are 0-based! The parser has already taken care of that.
applyInstruction :: Bool -> [String] -> (Int, Int, Int) -> [String]
applyInstruction applyReverse stacks (nrOfCrates, from, to) = newFirstPart ++ [newStackWithLowIdx] ++ newMiddlePart ++ [newStackWithHighIdx] ++ newLastPart
  where (newFirstPart, stackWithLowIdx, newMiddlePart, stackWithHighIdx, newLastPart) = splitAtIndices from to stacks
        headFrom = if (applyReverse) then reverse $ take nrOfCrates $ stacks!!from else take nrOfCrates $ stacks!!from
        tailFrom = drop nrOfCrates $ stacks!!from
        toStack = stacks!!to
        newStackWithLowIdx = if from < to then tailFrom else headFrom ++ toStack
        newStackWithHighIdx = if from < to then headFrom ++ toStack else tailFrom

-- Function to apply a list of instructions
applyInstructions :: Bool -> [String] -> [(Int, Int, Int)] -> [String]
applyInstructions applyReverse list [] = list
applyInstructions applyReverse list (x:xs) = applyInstructions applyReverse result xs
  where result = applyInstruction applyReverse list x

