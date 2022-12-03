import Data.List
import Data.Maybe

main ::  IO()
main = do
  filecontent <- readFile "D:\\Haskell\\MyPrograms\\AoC_2022\\AoCDay03.txt"
  let inputLines = lines filecontent
  let sameElements = map head $ map commonElements $ map chopList $ inputLines
  let answer1 = sum [priority x| x <- sameElements]
  print answer1

  let lines2 = groupsOf3 inputLines
  let common = map commonElements2 $ lines2
  let answer2 = sum [priority $ head x | x <- common ]
  print answer2

halfLength :: [a] -> Int
halfLength l = length(l) `div` 2

chopList :: [a] -> ([a],[a])
chopList [] = ([],[])
chopList l = (take (halfLength l) l, drop (halfLength l) l)

commonElements :: (Eq a) => ([a], [a]) -> [a]
commonElements (l1,l2) = [x|x <- l1, y<-l2, x == y]

priorities = ['a'..'z'] ++ ['A'..'Z']

priority c = 1 + fromJust( elemIndex c priorities )

groupsOf3  :: [a] -> [[a]]
groupsOf3 [] = []
groupsOf3 (a:b:c:xs) = [a,b,c]:groupsOf3 xs

commonElements2 :: (Eq a) => [[a]] -> [a]
commonElements2 [l1,l2,l3] = [x|x<-l1, y<-l2, x==y, z<-l3, z==x]