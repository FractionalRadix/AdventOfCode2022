import Data.Char

main ::  IO()
main = do
  filecontent <- readFile "D:\\Haskell\\MyPrograms\\AoC_2022\\AoCDay06.txt"
  let inputLines = lines filecontent
  print inputLines
  print $ startOfPacket $ inputLines!!0  --  7
  --print $ startOfPacket $ inputLines!!1  --  5
  --print $ startOfPacket $ inputLines!!2  --  6
  --print $ startOfPacket $ inputLines!!3  -- 10
  --print $ startOfPacket $ inputLines!!4  -- 11

startOfPacket :: String -> Int
startOfPacket str = 3 + firstFourDifferent str

fourDifferentParameters :: (Ord a) => a -> a -> a -> a -> Bool
fourDifferentParameters a b c d = a /= b && a /= c && a /= d && b /= c && b /= d && c /= d


firstFourDifferent :: (Ord a) => [a] -> Int
firstFourDifferent (x:y:z:w:xs)  
 | fourDifferentParameters x y z w   = 1
 | otherwise                         = 1 + firstFourDifferent (y:z:w:xs)

-- Test if the elements in a list are all different.
-- In other words, test if a list does NOT contain duplicates.
allDifferent :: (Ord a) => [a] -> Bool
allDifferent [] = True
allDifferent (x:xs) = not (x `elem` xs) && allDifferent xs

