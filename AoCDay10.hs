import Data.List

main ::  IO()
main = do
  filecontent <- readFile "D:\\Haskell\\MyPrograms\\AoC_2022\\AoCDay10_sample2.txt"
  let inputLines = lines filecontent
  print $ zip [1..] inputLines
  let instructions = map parseLine inputLines
  print $ instructions
  
  let values = scanl applyInstruction (0,1) instructions
  print values

  let measurePoints = [20, 40 ..]
  
  --let results = zipWith signalStrengthCycleN measurePoints values
  --print results
  
  print $ signalStrengthCycleN 20 values
  print $ signalStrengthCycleN 60 values
  print $ signalStrengthCycleN 100 values
  print $ signalStrengthCycleN 140 values
  print $ signalStrengthCycleN 180 values
  print $ signalStrengthCycleN 220 values

  --print (answer1 list)
  --print (answer2 list)

parseLine :: String -> (Int, Int)
parseLine "noop" = (1, 0)
parseLine str = (2, read (drop 5 str) :: Int)

applyInstruction :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
applyInstruction (cycleNr, x) (deltaCycleNr, deltaX) = (cycleNr + deltaCycleNr, x + deltaX)

-- Given a list of numbers and an upper bound n, find the highest value in that list that is still below or equal to n.
highestValueBelow :: (Ord a) => a -> [a] -> a
highestValueBelow n l = maximum [x| x <-l, x < n]

signalStrengthCycleN :: Int -> [(Int, Int)] -> Int
signalStrengthCycleN n pairList = n * xValue
   where cycleNumbers = map fst pairList
         cycleNr' = highestValueBelow n cycleNumbers
         tuplesForCycleNr = [(c1,x1)| (c1,x1) <- pairList, c1 == cycleNr']
         (cycleNr, xValue) = head tuplesForCycleNr
