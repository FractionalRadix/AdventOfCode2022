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


-- execute command-list current-directory current-filesystem -> remaining-command-list updated-current-directory new-filesystem
--execute :: [String] -> [String] -> FSEntry -> [String] -> [String] -> FSEntry
--execute [] curDir curFS = [] curDir curFS  -- hopefully that won't result in an endless loop somewhere.... see if I can prevent that.
--execute (cmdHead:cmdTail) curDir curFS = 
  --if "$ ls" `isPrefixOf` cmdHead
  --then executeLs cmdTail curDir curFS
  --else if "$ cd .." `isPrefixOf` cmdHead then executeOneDirectoryUp cmdTail curDir curFS
  --else if "$ cd /" `isPrefixOf` cmdHead then executeGoToRootDir cmdTail curDir curFS
  --else if "$ cd " `isPrefixOf` cmdHead then execute executeCd' cmdTail curDir curFS
  --else cmdTail curDir dummyFS --TODO!~ 

-- According to ":t" the type is... "(t1 -> FSEntry -> t2) -> t1 -> p -> t2" ...??
--   ---> Because you're trying to return basically a function (3-part thing).
--        Maybe make a "State" class? State = remaining-command-list current-dir current-fs . Or just a Tuple...
--executeLs :: ([String] -> [String] -> FSEntry) -> ([String] -> [String] -> FSEntry)
--executeLs commandList curDir curFS = commandList curDir dummyFS  --TODO!+ Probably some "takeWhile (head cmd /= '$')" for a list of FSEntry, then parse all those entries and add them to the new file system.
executeLs :: [String] -> [String] -> FSEntry -> ([String], FSEntry)
executeLs commandList curDir curFS = (remainingCommands, Directory "dummy" parsedEntries)
  where entries = takeWhile (\x -> head(x) /= '$') commandList
        parsedEntries = map parseEntry entries
        remainingCommands = dropWhile (\x -> head (x) /= '$') commandList

testCmdList = ["dir a", "14848514 b.txt", "8504156 c.dat", "dir d", "$ cd a"]
testLs = executeLs testCmdList ["/"] (Directory "/" [])
--TODO!+ executeGoToRootDir commandList curDir curFS = command curDir dummyFS --TODO!+
--TODO!+ executeOneDirectoryUp commandList curDir curFS = commandList curDir dummyFS  --TODO!+
--TODO!+ executeCd' commandList curDir curFS = commandList curDir dummyFS -- TODO!+

dummyFS = (File "dummy" 2048)
--TODO!- Used as a placeholder while building the functions.

parseEntry :: String -> FSEntry
parseEntry str =
    if firstPart == "dir" 
    then (Directory secondPart [])
    else (File secondPart (read $ firstPart :: Int))
  where firstPart = takeWhile (/= ' ') str
        secondPart = tail $ dropWhile (/= ' ') str

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
-- addFile
--------------------------------------------------------------------------------------------------------------------------

-- addFile: copy the file system, if the current directory is precisely a given subfolder, then add a file as well.
addFile :: [String] -> FSEntry -> FSEntry -> FSEntry
addFile _                     _       (File      name size    ) = File name size -- Can't add a file to a file, only to a directory.
addFile [targetDirectoryName] newFile (Directory name contents) = 
    if targetDirectoryName == name
    then Directory name (newFile:newContents)
    else Directory name (newContents)
  where newContents = map (copyFS) contents  -- Note that since we're at "[targetDirectoryName]" the subdirs aren't on the path, that path is now "[]" ....
addFile (x:xs) newFile (Directory name contents) = Directory name newContents
  where newContents = map (addFile xs newFile) contents

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


actual3 = addFile ["d1", "d12", "d122"] (File "fa.txt" 2048) testdir
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

