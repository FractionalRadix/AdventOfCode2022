import Data.List

main ::  IO()
main = do
  filecontent <- readFile "D:\\Haskell\\MyPrograms\\AoC_2022\\AoCDay01.txt"
  let list = map sum $ parse $ split $ lines filecontent
  --print list
  print (answer1 list)
  print (answer2 list)


-- Given a list of Ints that are represented as Strings, return the corresponding list of Ints.  
listParse :: [String] -> [Int]
listParse = map read

-- Split function adapted from this answer: https://stackoverflow.com/a/46595679/812149
split l = case break (=="") l of
                (a, "":b) -> a : split b
                (a, [])    -> [a]

-- Given a list of list of Strings, parse it to a list of list of Int
parse :: [[String]] -> [[Int]]
parse [] = []
parse (x:xs) = (listParse x) : parse xs

-- Answer to the first question
answer1 list = maximum list

-- Answer to the second question
answer2 list = sum $ take 3 $ reverse $ sort list

testInput = ["1000","2000","3000","","4000","","5000","6000","","7000","8000","9000","","10000"]
