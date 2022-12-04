main ::  IO()
main = do
  filecontent <- readFile "D:\\Haskell\\MyPrograms\\AoC_2022\\AoCDay02.txt"
  print $ sum $ map score $ map parseLine $ lines filecontent 
  print $ sum $ map score2 $ map parseLine $ lines filecontent

parseLine :: String -> (Char, Char)
parseLine l = (head l, last l)

score :: (Char, Char) -> Int
score (a, b) = (scoreABC a b) + (scoreXYZ b)

scoreXYZ :: Char -> Int
scoreXYZ 'X' = 1 
scoreXYZ 'Y' = 2
scoreXYZ 'Z' = 3

scoreABC :: Char -> Char -> Int
scoreABC 'A' 'X' = 3
scoreABC 'A' 'Y' = 6
scoreABC 'A' 'Z' = 0

scoreABC 'B' 'X' = 0
scoreABC 'B' 'Y' = 3
scoreABC 'B' 'Z' = 6

scoreABC 'C' 'X' = 6
scoreABC 'C' 'Y' = 0
scoreABC 'C' 'Z' = 3

scoreXYZ2 :: Char -> Char -> Int
scoreXYZ2 _ 'X' = 0
scoreXYZ2 _ 'Y' = 3
scoreXYZ2 _ 'Z' = 6

scoreABC2 :: Char -> Char -> Int
-- opponent plays rock
scoreABC2 'A' 'X' =  3 -- scissors
scoreABC2 'A' 'Y' =  1 -- rock
scoreABC2 'A' 'Z' =  2 -- paper

-- opponent plays paper
scoreABC2 'B' 'X' =  1 -- rock
scoreABC2 'B' 'Y' =  2 -- paper
scoreABC2 'B' 'Z' =  3 -- scissors

-- opponent plays siccors
scoreABC2 'C' 'X' =  2 --paper
scoreABC2 'C' 'Y' =  3 --scissors
scoreABC2 'C' 'Z' =  1 --rock

score2 :: (Char, Char) -> Int
score2 (a, b) = scoreABC2 a b + scoreXYZ2 a b
 

