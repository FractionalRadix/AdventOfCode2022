import Data.Char

main ::  IO()
main = do
  filecontent <- readFile "D:\\Haskell\\MyPrograms\\AoC_2022\\AoCDay06.txt"
  let inputLines = lines filecontent
  print inputLines
  let answer1 = startOfPacket $ inputLines!!0
  print $ "First packet marker: " ++ show answer1
  let answer2 = startOfMessage $ inputLines!!0
  print $ "First message marker: " ++ show answer2
  --print $ startOfPacket $ inputLines!!0  --  7
  --print $ startOfPacket $ inputLines!!1  --  5
  --print $ startOfPacket $ inputLines!!2  --  6
  --print $ startOfPacket $ inputLines!!3  -- 10
  --print $ startOfPacket $ inputLines!!4  -- 11
  --print $ startOfMessage $ inputLines!!0  --  19
  --print $ startOfMessage $ inputLines!!1  --  23
  --print $ startOfMessage $ inputLines!!2  --  23
  --print $ startOfMessage $ inputLines!!3  -- 29
  --print $ startOfMessage $ inputLines!!4  -- 26


startOfPacket :: String -> Int
startOfPacket str = 3 + firstFourDifferent str

startOfMessage :: String -> Int
startOfMessage str = 13 + firstFourteenDifferent str

firstFourDifferent :: (Ord a) => [a] -> Int
firstFourDifferent (x:xs)  = if (allDifferent (take 4 (x:xs))) then 1 else 1 + firstFourDifferent xs

firstFourteenDifferent :: (Ord a) => [a] -> Int
firstFourteenDifferent (x:xs) = if (allDifferent (take 14 (x:xs))) then 1 else 1 + firstFourteenDifferent xs

-- Test if the elements in a list are all different.
-- In other words, test if a list does NOT contain duplicates.
allDifferent :: (Ord a) => [a] -> Bool
allDifferent [] = True
allDifferent (x:xs) = not (x `elem` xs) && allDifferent xs

