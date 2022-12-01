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
--answer1 filecontent = maximum $ map sum $ parse $ split $ lines filecontent
answer1 list = maximum list

secondHighest :: [Int] -> Int
secondHighest l = maximum (filter ( < (maximum l) ) l)

thirdHighest :: [Int] -> Int
thirdHighest l = maximum (filter ( < (secondHighest l) ) l)

-- There are certainly more efficient ways to do this kind of thing. But this gets the job done in time.
answer2 list = maximum list + secondHighest list + thirdHighest list

testInput = ["1000","2000","3000","","4000","","5000","6000","","7000","8000","9000","","10000"]
