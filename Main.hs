import System.Directory
import System.IO

import System.INotify
import System.Directory(getDirectoryContents)
import Control.Monad(when,filterM)
import Data.List(isPrefixOf)
import Text.Regex.TDFA

import Database.Redis
import Indexer(indexFilePath)

main :: IO ()
main = do
    inotify <- initINotify
    print inotify
    home <- getHomeDirectory        

    directories <- subdirectories (home)
    --index
    reditConn <- connect defaultConnectInfo
    
    --mapM $ (\fp -> runRedis reditConn $ do
    --  set 
    
    {-wd <- mapM (\d -> addWatch
            inotify
            [AllEvents]
            d
            --updateIndex 
            print) directories-}
    
    --print wd
    mapM_ (flip indexFilePath reditConn) directories
    putStrLn "Listens to your home directory. Hit enter to terminate."
    getLine
    print ""
    --mapM_ removeWatch wd  

updateIndex :: Event -> IO ()
updateIndex (Accessed isDirectory mfilePath) = putStrLn "Update access date"
updateIndex (Created isDirectory mfilePath) = putStrLn "Create file"
updateIndex e  = putStrLn $ "Unhandled event " ++ (show e)

-- | Gives a list of subdirectories given a directory
subdirectories :: FilePath -> IO [FilePath]
subdirectories fp = do
  isDir <- doesDirectoryExist fp
  isIgnored <- isIgnored fp
  if (isDir && not isIgnored)
  then do 
       children' <- getDirectoryContents fp -- :: [FilePath]
       let 
          children = filter (`notElem` [".","..","_darcs",".config",".cabal"]) children'
          childrenFilePaths = map (fp </>) children
 
       directoryFilePaths <- filterM (doesDirectoryExist) childrenFilePaths
                   
       allDescendants <- mapM subdirectories directoryFilePaths 
       return $ directoryFilePaths ++ concat allDescendants
  else return []
   
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
 
