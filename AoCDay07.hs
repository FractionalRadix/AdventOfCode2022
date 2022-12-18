import Data.List

main ::  IO()
main = do
  filecontent <- readFile "D:\\Haskell\\MyPrograms\\AoC_2022\\AoCDay07_sample.txt"
  let inputLines = lines filecontent
  print $ zip [1..] inputLines
  --let answer1 = 
  --print $ "First packet marker: " ++ show answer1
  --let answer2 = 
  --print $ "First message marker: " ++ show answer2

  print $ replaceElt "whitefrag" 6 'l' -- expected: whiteflag

  let currentDirectory = executeCdList [] ["$ cd /", "$ cd this", "$ cd is", "$ cd ..", "$ cd was","$ cd /", "$ cd that", "$ cd will", "$ cd work"]
  print currentDirectory


-- A directory is a list of files and directories.
-- Let's call "file or directory" an FSEntry (File System Entry).
data FSEntry = File String Int | Directory String [FSEntry] deriving (Show, Eq)

-- Given a File System Entry (whether file or directory), get the name of that entry.
entryName :: FSEntry -> String
entryName (Directory name _) = name
entryName (File      name _) = name

--------------------------------------------------------------------------------------------------------------------------
-- selectDirectory, unJust
--------------------------------------------------------------------------------------------------------------------------

-- Given a directory and the name of a direct subdirectory, select that subdirectory.
selectDirectory :: String -> FSEntry -> Maybe FSEntry
selectDirectory subdirectoryName (Directory folderName xs) = find (\x -> entryName x == subdirectoryName) xs

-- Dirty trick, but I haven't gotten to the chapters on lifting yet :sweat-smile:
unJust :: Maybe FSEntry -> FSEntry
unJust (Just x) = x

actual1 = unJust $ selectDirectory "Donald" (Directory "ducks" [(Directory "Scrooge" []),(Directory "Donald" [(File "Huey" 1),(File "Dewey" 2),(File "Louie" 3)]), (Directory "Daisy" [])])
expected1 = (Directory "Donald" [(File "Huey" 1),(File "Dewey" 2),(File "Louie" 3)])
test1 = testFS actual1 expected1

--------------------------------------------------------------------------------------------------------------------------
-- copyFS, copyFSbutRenameFile
--------------------------------------------------------------------------------------------------------------------------

copyFS :: FSEntry -> FSEntry
copyFS (File      name size    ) = File name size
copyFS (Directory name contents) = Directory name contents

copyFSbutRenameFile :: String -> String -> FSEntry -> FSEntry
copyFSbutRenameFile oldName newName (File name size) = 
  if name == oldName 
  then File newName size
  else File name size
copyFSbutRenameFile oldName newName (Directory name contents) = Directory name $ map (copyFSbutRenameFile oldName newName) contents

actual2   = copyFSbutRenameFile "old" "new" (Directory "d1" [Directory "d11" [File "old" 1024], Directory "d12"[File "same" 2048 , File "old" 8192]])
expected2 = Directory "d1" [Directory "d11" [File "new" 1024], Directory "d12"[File "same" 2048 , File "new" 8192]]
test2 = testFS actual2 expected2

--------------------------------------------------------------------------------------------------------------------------
-- copyFSandAddFileInSpecificDirectory
--------------------------------------------------------------------------------------------------------------------------

-- copyFSandAddFileInSpecificDirectory: copy the file system, if the current directory is precisely a given subfolder, then add a file as well.
copyFSandAddFileInSpecificDirectory :: [String] -> FSEntry -> FSEntry -> FSEntry
copyFSandAddFileInSpecificDirectory _                     _       (File      name size    ) = File name size -- Can't add a file to a file, only to a directory.
copyFSandAddFileInSpecificDirectory [targetDirectoryName] newFile (Directory name contents) = 
    if targetDirectoryName == name
    then Directory name (newFile:newContents)
    else Directory name (newContents)
  where newContents = map (copyFS) contents  -- Note that since we're at "[targetDirectoryName]" the subdirs aren't on the path, that path is now "[]" ....
copyFSandAddFileInSpecificDirectory (x:xs) newFile (Directory name contents) = Directory name newContents
  where newContents = map (copyFSandAddFileInSpecificDirectory xs newFile) contents

testdir = Directory "d1" 
  [  Directory "d11" [], 
     Directory "d12" 
     [  Directory "d121" [],
        Directory "d122" [Directory "d1221" [], Directory "d1222" []],
        Directory "d123" []
     ], 
     Directory "d13" []
  ]
testfile1 = File "fa.txt" 1024


actual3 = copyFSandAddFileInSpecificDirectory ["d1", "d12", "d122"] (File "fa.txt" 2048) testdir
expected3 = Directory "d1" 
  [  Directory "d11" [], 
     Directory "d12" 
     [  Directory "d121" [],
        Directory "d122" [File "fa.txt" 2048,Directory "d1221" [], Directory "d1222" []],
        Directory "d123" []
     ], 
     Directory "d13" []
  ]
test3 = testFS actual3 expected3


--------------------------------------------------------------------------------------------------------------------------
-- stuff under construction...
--------------------------------------------------------------------------------------------------------------------------

testFS :: FSEntry -> FSEntry -> Bool
testFS t1 t2 = t1 == t2

replaceElt :: [a] -> Int -> a -> [a]
replaceElt l n x = (take n l) ++ [x] ++ (drop (n+1) l)

-- Parse a single "CD" command.
-- The current path is passed as an array of String, where each element is a directory.
-- For example, "/this/is/my/path" would be represented as ["this", "is", "my", "path"].
executeCd :: [String] -> String -> [String]
executeCd path "$ cd /"  = []
executeCd path "$ cd .." = init path
executeCd path ('$':' ':'c':'d':' ':xs) = path ++ [xs]

executeCdList path [] = path
executeCdList path (x:xs) = executeCdList (executeCd path x) xs

--testParseCd = parseCd [] ["$ cd /", "$ cd this", "$ cd is", "$ cd ..", "$ cd was","$ cd /", "$ cd that", "$ cd will", "$ cd work"]
  --  /this/is
  --       /was
  --  /that
  --       /will
  --            /work
  -- [[this, was], [that, [will, [work]]

parseLs :: [String] -> [String]
parseLs list = takeWhile (\elt -> head(elt) /= '$') list

