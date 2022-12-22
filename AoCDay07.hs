import Data.List

main ::  IO()
main = do
  filecontent <- readFile "D:\\Haskell\\MyPrograms\\AoC_2022\\AoCDay07.txt"
  --let inputLines = lines filecontent
  let inputLines = mcve2
  --print $ zip [1..] inputLines

  let filesystem = executeList (inputLines, [], (Directory "/"[]))
  print $ filesystem

  let totalSize = fsEntrySize filesystem
  print totalSize

  let annotedFileSystem = calculateSizes filesystem
  print annotedFileSystem

  let dirSizes = directorySizes annotedFileSystem
  print dirSizes

  let smallSizes = [n|(_,n) <- dirSizes, n <=  100000]
  print smallSizes

  let answer1 = sum smallSizes
  print $ "The sum of all directories whose size is less than  100000 is: " ++ show answer1

  let printableTree = printFSEntry 2 annotedFileSystem
  putStr $ unlines printableTree
  

-- I got "1949099" and that's too high...
-- The problem is that some entries are DUPLICATED.
-- Not sure why, but maybe if you enter the same directory twice a new entry is made?
mcve1 = ["$ cd /", "$ ls", "dir a", "dir b", "$ cd a", "$ ls", "1024 a.txt", "$ cd ..", "$ cd a"]  
-- CONFIRMED: if a directory appears at the same level with the same name, ALL entries of ALL these directories are added.
mcve2 = ["$ cd /", "$ ls", "dir dir1", "dir dir2", "$ cd dir1", "$ ls", "dir subdir", "$ cd subdir", "$ ls", "1024 a.txt", "$ cd ..", "$ cd ..", "$ cd dir2", "$ ls", "dir subdir", "$ cd subdir", "$ ls", "2048 b.txt"]
-- /
--       dir1
--         subdir
--           a.txt
--       dir2
--         subdir
--           b.txt
-- This input will add "b.txt" to BOTH cases of "subdir"!    
-- But ONLY because the names are the same!
-- Hence, the problem must be in executeLs, and likely in its call to "addFSEntries" which it uses. Time to manually trace that..


-- A directory is a list of files and directories.
-- Let's call "file or directory" an FSEntry (File System Entry).
data FSEntry = File String Int | Directory String [FSEntry] deriving (Show, Eq)

data FSEntryWithSize = FileWithSize String Int | DirectoryWithSize String Int [FSEntryWithSize] deriving (Show, Eq)

spaces :: Int -> String
spaces n = replicate n ' '

-- Turn an FS Entry into a list of strings, with indentation.
printFSEntry :: Int -> FSEntryWithSize -> [String]
printFSEntry indentation (FileWithSize name size) = [(spaces indentation) ++ name ++ " (file, size=" ++ (show size) ++ ")"]
printFSEntry indentation (DirectoryWithSize name _ contents) = (myHead:myTail)
  where myHead = (replicate indentation ' ') ++ name ++ " (dir)"
        myTail = foldl (++) [] $ map (printFSEntry (indentation + 2)) contents


directorySizes :: FSEntryWithSize -> [(String, Int)]
directorySizes (FileWithSize _ _) = []
directorySizes (DirectoryWithSize name size contents) = [(name, size)] ++ directorySizesList contents

directorySizesList :: [FSEntryWithSize] -> [(String, Int)]
directorySizesList [] = []
directorySizesList (x:xs) = head ++ tail
  where head = directorySizes x
        tail = directorySizesList xs


calculateSizes :: FSEntry -> FSEntryWithSize
calculateSizes (File name size) = (FileWithSize name size)
calculateSizes (Directory name contents) = (DirectoryWithSize name size sizedContents)
  where sizedContents = map calculateSizes contents
        size = sum (map getSize sizedContents)

getSize :: FSEntryWithSize -> Int
getSize (FileWithSize _ n) = n
getSize (DirectoryWithSize _ n _) = n

fsEntrySize :: FSEntry -> Int
fsEntrySize (File _ n) = n
fsEntrySize (Directory _ contents) = sum $ map fsEntrySize contents

-- execute command-list current-directory current-filesystem -> remaining-command-list updated-current-directory new-filesystem
-- Note that we do NOT provide a handler for the situation where the command list is empty. That situation is deliberately not defined.
execute :: ([String], [String], FSEntry) -> ([String], [String], FSEntry)
execute ((cmdHead:cmdTail), curDir, curFS) = 
  if "$ ls" `isPrefixOf` cmdHead then executeLs (cmdTail, curDir, curFS)
  else if "$ cd .." `isPrefixOf` cmdHead then executeOneDirectoryUp (cmdTail, curDir, curFS)
  else if "$ cd /" `isPrefixOf` cmdHead then executeGoToRootDir (cmdTail, curDir, curFS)
  else if "$ cd " `isPrefixOf` cmdHead then executeCd (drop 5 cmdHead) (cmdTail, curDir, curFS)
  else error "Unknown AoC Communicator command" --TODO?- Remove when the code is ready for all cases?

-- executeList command-list current-directory current-filesystem -> new-filesystem
executeList :: ([String], [String], FSEntry) -> FSEntry
executeList ([], curDir, curFS) = curFS
executeList ((cmdHead:cmdTail), curDir, curFS) = 
  if "$ ls" `isPrefixOf` cmdHead then 
     executeList $ executeLs (cmdTail, curDir, curFS)
  else if "$ cd .." `isPrefixOf` cmdHead then 
     executeList $ executeOneDirectoryUp (cmdTail, curDir, curFS)
  else if "$ cd /" `isPrefixOf` cmdHead then 
     executeList $ executeGoToRootDir (cmdTail, curDir, curFS)
  else if "$ cd " `isPrefixOf` cmdHead then 
     executeList $ executeCd (drop 5 cmdHead) (cmdTail, curDir, curFS)
  else 
     error "Unknown AoC Communicator command" --TODO?- Remove when the code is ready for all cases?

executeLs :: ([String], [String], FSEntry) -> ([String], [String], FSEntry)
executeLs (commandList, curDir, curFS) = (remainingCommands, curDir, newFS)
  where entries = takeWhile (\x -> head(x) /= '$') commandList
        parsedEntries = map parseEntry entries
        remainingCommands = dropWhile (\x -> head (x) /= '$') commandList
        newFS = addFSEntries curDir parsedEntries curFS

testCmdList = ["dir a", "14848514 b.txt", "8504156 c.dat", "dir d", "$ cd a"]
testLs = executeLs (testCmdList, ["/"], (Directory "/" []))

executeOneDirectoryUp (cmdList, currentPath, fs) = (cmdList, init currentPath, fs)

actual4   = executeOneDirectoryUp (["ls"], ["/", "you", "are", "here"], (File "dummy" 2048))
expected4 = (["ls"], ["/", "you", "are"], (File "dummy" 2048))
test4 = actual4 == expected4

executeGoToRootDir :: ([String], [String], FSEntry) -> ([String], [String], FSEntry)
executeGoToRootDir (cmdList, currentPath, fs) = (cmdList, ["/"], fs) -- Not sure if it should go to {"/"]  or to [] ...

executeCd :: String -> ([String], [String], FSEntry) -> ([String], [String], FSEntry)
executeCd newDir (cmdList, currentPath, fs) = (cmdList, currentPath ++ [newDir], fs) --TODO?+ Add a check to see if that directory is present...?

parseEntry :: String -> FSEntry
parseEntry str =
    if firstPart == "dir" 
    then (Directory secondPart [])
    else (File secondPart (read $ firstPart :: Int))
  where firstPart = takeWhile (/= ' ') str
        secondPart = tail $ dropWhile (/= ' ') str

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
-- addFSEntry
--------------------------------------------------------------------------------------------------------------------------

-- addFSEntry: copy the file system, if the current directory is precisely a given subfolder, then add a given FSEntry (File or Directory) as well.
-- addFSEntry :: path, file or directory to add, current file system -> new file system
addFSEntry :: [String] -> FSEntry -> FSEntry -> FSEntry
addFSEntry _                     _       (File      name size    ) = File name size -- Can't add a file to a file, only to a directory.
addFSEntry [targetDirectoryName] newFile (Directory name contents) = 
    if targetDirectoryName == name
    then Directory name (newFile:newContents)
    else Directory name (newContents)
  where newContents = map (copyFS) contents  -- Note that since we're at "[targetDirectoryName]" the subdirs aren't on the path, that path is now "[]" ....
addFSEntry (x:xs) newFile (Directory name contents) = Directory name newContents
  where newContents = map (addFSEntry xs newFile) contents

--TODO?+ Add unit tests?
addFSEntries :: [String] -> [FSEntry] -> FSEntry -> FSEntry
addFSEntries _ _ (File name size) = File name size -- Can't add files or directories to files, only to directories.
addFSEntries [targetDirectoryName] newEntries (Directory name contents) =
    if targetDirectoryName == name
    then Directory name (newEntries ++ newContents)
    else Directory name newContents
  where newContents = map (copyFS) contents
addFSEntries (x:xs) newFSEntries (Directory name contents) = Directory name newContents
  where newContents = map (addFSEntries xs newFSEntries) contents

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


actual3 = addFSEntry ["d1", "d12", "d122"] (File "fa.txt" 2048) testdir
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

-- Parse a single "CD" command.
-- The current path is passed as an array of String, where each element is a directory.
-- For example, "/this/is/my/path" would be represented as ["this", "is", "my", "path"].
executeCd' :: [String] -> String -> [String]
executeCd' path "$ cd /"  = []
executeCd' path "$ cd .." = init path
executeCd' path ('$':' ':'c':'d':' ':xs) = path ++ [xs]

executeCdList path [] = path
executeCdList path (x:xs) = executeCdList (executeCd' path x) xs

--testParseCd = parseCd [] ["$ cd /", "$ cd this", "$ cd is", "$ cd ..", "$ cd was","$ cd /", "$ cd that", "$ cd will", "$ cd work"]
  --  /this/is
  --       /was
  --  /that
  --       /will
  --            /work
  -- [[this, was], [that, [will, [work]]

parseLs :: [String] -> [String]
parseLs list = takeWhile (\elt -> head(elt) /= '$') list

