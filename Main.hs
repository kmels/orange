import System.Directory
import System.IO

import System.INotify
import System.Directory(getDirectoryContents)
import Control.Monad(when,filterM)

main :: IO ()
main = do
    inotify <- initINotify
    print inotify
    home <- getHomeDirectory
    indices <- subdirectories home
    wd <- addWatch
            inotify
            [AllEvents]
            home
            --updateIndex 
            print
    
    print wd
    putStrLn "Listens to your home directory. Hit enter to terminate."
    getLine
    removeWatch wd
    
updateIndex :: Event -> IO ()
updateIndex (Accessed isDirectory mfilePath) = putStrLn "Update access date"
updateIndex (Created isDirectory mfilePath) = putStrLn "Create file"
updateIndex e  = putStrLn $ "Unhandled event " ++ (show e)

-- | Gives a list of subdirectories given a directory
subdirectories :: FilePath -> IO [FilePath]
subdirectories fp = do    
  putStrLn $ "****************************************" ++ "Computing subfiles " ++ fp
  isDir <- doesDirectoryExist fp  
  if isDir
  then 
      case fp of
        ".." -> return $ []
        "." -> return $ []
        _ -> do 
          children' <- getDirectoryContents fp -- :: [FilePath]
          let 
            children = filter (`notElem` [".","..","_darcs",".config",".cabal"]) children'
            childrenFilePaths = map (</> fp) children

          directoryFilePaths <- filterM (doesDirectoryExist) childrenFilePaths
            
          allDescendants <- mapM subdirectories directoryFilePaths 
          return $ directoryFilePaths ++ concat allDescendants
  else return [fp]
   
(</>) :: FilePath -> FilePath -> FilePath
(</>) cfp parent = parent ++ "/" ++ cfp

