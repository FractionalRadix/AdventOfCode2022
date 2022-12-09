import Data.Char

main ::  IO()
main = do
  filecontent <- readFile "D:\\Haskell\\MyPrograms\\AoC_2022\\AoCDay05_sample.txt"
  let inputLines = lines filecontent
  let stackLines = takeWhile containsCrate inputLines
  print $  zip [1..] stackLines
  let instructions = tail $ dropWhile (/= []) inputLines
  print $ zip [1..] instructions
  let allStacks = map trimSpaces $ map (stackNumberN stackLines) [1..3]
  print $ zip [1..] allStacks
  let parsedInstructions = map parseInstruction instructions
  print $ zip [1..] parsedInstructions

  let alphabetStrings = map (\c -> [c]) ['a'..'z']
  print alphabetStrings
  let splitExample = splitAtIndices 3 9 alphabetStrings
  print $ splitExample

  -- First step towards following the first instruction...
  print allStacks
  print (parsedInstructions!!0)
  let firstStep = applyInstruction allStacks (parsedInstructions!!0)
  print $ firstStep

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

-- For now, assume all instructions only move ONE crate. We'll deal with multiple crates later.

-- Note that the instructions here are 0-based...
-- TODO!~ This is SWAPPING heads not MOVING heads...
applyInstruction :: [String] -> (Int, Int, Int) -> [String]
applyInstruction stacks (_, from, to) = newFirstPart ++ [newStackWithLowIdx] ++ newMiddlePart ++ [newStackWithHighIdx] ++ newLastPart
  where (newFirstPart, stackWithLowIdx, newMiddlePart, stackWithHighIdx, newLastPart) = splitAtIndices from to stacks
        headFrom = head (stacks!!from)
        tailFrom = tail (stacks!!from)
        toStack = stacks!!to
        newStackWithLowIdx = if from < to then tailFrom else (headFrom:toStack)
        newStackWithHighIdx = if from < to then (headFrom:toStack) else tailFrom

