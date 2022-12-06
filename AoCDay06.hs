import Data.Char

main ::  IO()
main = do
  filecontent <- readFile "D:\\Haskell\\MyPrograms\\AoC_2022\\AoCDay06.txt"
  let inputLines = lines filecontent
  let answer1 = startOfPacket $ inputLines!!0
  print $ "First packet marker: " ++ show answer1
  let answer2 = startOfMessage $ inputLines!!0
  print $ "First message marker: " ++ show answer2

startOfPacket :: String -> Int
startOfPacket str = 3 + firstNDifferent 4 str

startOfMessage :: String -> Int
startOfMessage str = 13 + firstNDifferent 14 str

-- Find the first segment of length n, where all elements are different.
firstNDifferent :: (Ord a) => Int -> [a] -> Int
firstNDifferent n (x:xs)  = if (allDifferent (take n (x:xs))) then 1 else 1 + firstNDifferent n xs

-- Test if the elements in a list are all different.
-- In other words, test if a list does NOT contain duplicates.
allDifferent :: (Ord a) => [a] -> Bool
allDifferent [] = True
allDifferent (x:xs) = not (x `elem` xs) && allDifferent xs

