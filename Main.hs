import System.Directory
import System.IO

import System.INotify
import System.Directory(getDirectoryContents)
import Control.Monad(when)

main :: IO ()
main = do
    inotify <- initINotify
    print inotify
    home <- getHomeDirectory
    indices <- subfiles home
    putStrLn $ "Finished! " ++ (show $ indices)
--    mapM_ $ putStrLn indices
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

subfiles :: FilePath -> IO [FilePath]
subfiles fp = do  
  putStrLn $ "****************************************" ++ "Computing subfiles " ++ fp
  isDir <- doesDirectoryExist fp  
  if isDir
  then 
      case fp of
        ".." -> return $ []
        "." -> return $ []
        _ -> do 
          children <- getDirectoryContents fp
          putStrLn $ "Computing children of " ++ (show children)
          allDescendants <- mapM subfiles children
          return $ concat allDescendants
  else return [fp]
  
  