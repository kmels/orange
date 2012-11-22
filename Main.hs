import System.Directory
import System.IO
import System.IO.Unsafe

import System.INotify
import System.Directory(getDirectoryContents)
import Control.Monad(when,filterM)
import Data.List(isPrefixOf,partition)
import Text.Regex.TDFA

import Database.Redis

import Indexer(indexFilePath)
import Search(searchFilePaths)

import System.Environment

{-  | Usage

orange index => indexes files + watches
orange watch => watches
orange search abc => output results 

-}
main :: IO ()
main = do  
    args <- getArgs    
    case args of 
      ["index"] -> do
        inotify <- initINotify
        print inotify
        home <- getHomeDirectory        

        --get list of home subdirectories
        (watchDescriptors, directories) <- subfiles home
        --indexes subdirectories filepaths
        reditConn <- connect defaultConnectInfo
    
        {-wd <- mapM (\d -> addWatch
            inotify
            [AllEvents]
            d
            --updateIndex 
            print) directories-}
    
        --print wd
        mapM_ (flip indexFilePath reditConn) directories
        putStrLn $ "I just indexed " ++ show (length directories) ++ " file paths"
        putStrLn "and, I'm now listening to all the changes that happen to them. Hit enter to terminate, because I want to keep my index update!."
        getLine
        
        print ""
      ["search",query] -> do
        conn <- connect defaultConnectInfo
        results <- searchFilePaths query conn Nothing
        mapM_ (putStrLn . show )results
      _ -> do
        putStrLn "Error, usage: `orange index` or `orange search <query`"
    --mapM_ removeWatch wd  

updateIndex :: Event -> IO ()
updateIndex (Accessed isDirectory mfilePath) = putStrLn "Update access date"
updateIndex (Created isDirectory mfilePath) = putStrLn "Create file"
updateIndex e  = putStrLn $ "Unhandled event " ++ (show e)

-- | Gives a list of subdirectories given a directory
subfiles :: FilePath -> IO ([FilePath],[FilePath])
subfiles fp = do
  isDir <- doesDirectoryExist fp
  isIgnored <- isIgnored fp
  if (isDir && not isIgnored)
  then do 
       children' <- getDirectoryContents fp -- :: [FilePath]
       let 
          children = filter (`notElem` [".",".."]) children'
          childrenFilePaths = map (fp </>) children
 
       let (directoryFilePaths, normalFilePaths) = partition (unsafePerformIO . doesDirectoryExist) childrenFilePaths
 
       allDescendants <- mapM subfiles directoryFilePaths
       
       return $ let                   
                  dirs = concatMap fst allDescendants
                  files = concatMap snd allDescendants       
                in (directoryFilePaths ++ dirs, normalFilePaths ++ files)
  else return ([],[])
       
watch :: FilePath -> INotify -> IO WatchDescriptor
watch dirFilePath inotify = addWatch
                 inotify
                 [AllEvents]
                 dirFilePath
                 print
   
(</>) :: FilePath -> FilePath -> FilePath
(</>) parent cfp = parent ++ "/" ++ cfp

-- | Checks for expressions in ~/.orange
isIgnored :: FilePath -> IO Bool
isIgnored fp = do
  homeDirectory <- getHomeDirectory
  ignoreRegex <- readFile $ homeDirectory </> ".orange"
  let matches = (any (fp =~) $ lines ignoreRegex)
  when matches $ putStrLn $ fp ++ " was ignored"
  --when (not matches) $ putStrLn " was not ignored"
  return $ matches


--  08005251378
--  1
  
 
