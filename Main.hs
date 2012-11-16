import System.Directory
import System.IO

import System.INotify

main :: IO ()
main = do
    inotify <- initINotify
    print inotify
    home <- getHomeDirectory
    wd <- addWatch
            inotify
            [AllEvents]
            (home ++ "/Downloads")
            print
    print wd
    putStrLn "Listens to your home directory. Hit enter to terminate."
    getLine
    removeWatch wd
    