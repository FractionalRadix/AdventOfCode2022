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
  let allInstructions = map parseInstruction instructions
  print $ zip [1..] allInstructions
  
  

containsCrate :: String -> Bool
containsCrate str = elem '[' str

stackNumberN :: [String] -> Int -> String
stackNumberN stacks n = map (!!p) stacks
  where p = 4 * n - 3

parseInstruction :: String -> (Int, Int, Int)
parseInstruction instr = (read $ tokens!!1, read $ tokens!!3, read $ tokens!!5)
  where tokens = split instr

-- This function adapted from https://stackoverflow.com/a/46595679/812149
split :: String -> [String]
split str = case break (==' ') str of
                (a, ' ':b) -> a : split b
                (a, "")    -> [a]

trimSpaces :: String -> String
trimSpaces str = dropWhile (==' ') str

-- Approach:
--   1. zip with [1.. nrOfStacks]
--   2. copy with guards:
--         idx == fromIdx - 1   = tail(str)
--         idx == toIdx - 1     = (movedCrate):str
--         otherwise            = str


--moveOneCrate :: Int -> Int -> [String] -> [String]
--moveOneCrate fromStackIdx toStackIdx stacks = [stacks!!0, stacks!!1, stacks!!2]
-- where
--   fromStack = stacks!!(fromStackIdx - 1)
--   toStack = stacks!!(toStacksIdx - 1)
--   fromStack' = tail fromStack
--   toStack' = (head fromStack):toStack
 -- apply something to the 10 stacks.
 -- create 10 new stacks, BUT:
 -- (fromStack-1) is now its tail, (toStack-1) has the head of (fromStack-1) added as its head.

-- Basically:
-- Copy every stack
--   EXCEPT if the index is (fromStackIdx-1)

   
