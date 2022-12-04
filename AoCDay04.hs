-- 536, 845

main ::  IO()
main = do
  filecontent <- readFile "D:\\Haskell\\MyPrograms\\AoC_2022\\AoCDay04.txt"
  let inputLines = lines filecontent
  let ranges  = map (lineToRanges) inputLines
  -- Solve problem #1
  let results1 = map (fullyContains) ranges
  let answer1 = length [x | x <-results1, x == True]
  print $ "Number of ranges fully including another range: " ++ (show answer1)
  -- Solve problem #2
  let results2 = map (hasOverlap) ranges 
  --let results11 = [x | x <- results10, x == True]
  let answer2 = length [x | x <- results2, x == True]
  print $ "Number of ranges with at least one overlapping element: " ++ (show answer2)

-- Given a string and a delimiter, take everything that comes before the first delimiter.
firstHalf :: Char -> String -> String
firstHalf c str = takeWhile (/=c) str

-- Given a string and a delimiter, take everything that comes after the first delimiter.
secondHalf :: Char -> String -> String
secondHalf c str = tail $ dropWhile (/=c) str

-- Given a string and a delimiter, split it in a part before the first delimiter and a part after the first delimiter.
bothHalves :: Char -> String -> (String, String)
bothHalves c str = (firstHalf c str, secondHalf c str)

lineToRanges :: String -> ((Int, Int), (Int, Int))
lineToRanges str = ((read startOfFirstRange, read endOfFirstRange), (read startOfSecondRange, read endOfSecondRange))
  where firstRange = firstHalf ',' str 
        secondRange = secondHalf ',' str 
        startOfFirstRange = firstHalf '-' $ firstRange 
        endOfFirstRange = secondHalf '-' $ firstRange
        startOfSecondRange = firstHalf '-' $ secondRange 
        endOfSecondRange = secondHalf '-' $ secondRange

fullyContains :: ((Int, Int), (Int, Int)) -> Bool
fullyContains ((s1,e1),(s2,e2)) = (s2 >= s1 && e2 <= e1) || (s1 >= s2 && e1 <= e2)

hasOverlap :: ((Int, Int), (Int, Int)) -> Bool
hasOverlap ((s1,e1),(s2,e2)) = (s1 <= e2 && e1 >= s2) || (s2 <= e1 && e2 >= s1)