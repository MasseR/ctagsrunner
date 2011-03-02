module Main where
import System.Process (proc, createProcess)
import System.INotify (withINotify, Event(..), EventVariety(..), addWatch)
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.FilePath ((</>), takeExtension)
import qualified Control.Monad.Stream as M
import qualified Data.List.Stream as L
import Data.Monoid(mempty)
import Control.Monad

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents path = do
  d <- (appendRoot path . L.filter (not . flip L.elem [".", "..", ".git"]))
    `fmap` (getDirectoryContents path)
    >>= M.filterM doesDirectoryExist
  next <- L.concat `fmap` (M.mapM getRecursiveContents) d
  return (d ++ next)
  where appendRoot a = L.map (a </>)

fields = "--fields=+iaS"
kinds = L.map (\(l,o) -> "--" ++ l ++ "-kinds=" ++ o) langs
  where langs = [("c++", "+p")]
ctags path = proc "ctags" ([fields] ++ kinds ++ ["-a", path])

getPath :: Event -> Maybe FilePath
getPath (Modified _ path) = path >>= acceptable
getPath (MovedIn _ path _) = acceptable path
getPath (MovedOut _ path _) = acceptable path
getPath (Created True _ ) = mempty -- Empty directory doesn't trigger
getPath (Created _ path ) = acceptable path
getPath (Deleted _ path ) = acceptable path

filetypes ::  [[Char]]
filetypes = [".php"]
acceptable :: FilePath -> Maybe FilePath
acceptable path | takeExtension path `elem` filetypes = return path
		| otherwise = mempty

inotify ::  FilePath -> Event -> IO ()
inotify root event =
  let maybepath = getPath event
  in case maybepath of
	  Nothing -> return ()
	  Just "tags" -> return ()
	  Just path -> do
	    putStrLn path
	    createProcess (ctags (root </> path))
	    return ()

main ::  IO ()
main = withINotify $ \i -> do
  dirs <- getRecursiveContents "."
  print dirs
  M.mapM_ (\f -> addWatch i [Modify, Delete] f (inotify f)) ("." : dirs)
  getLine -- Wait for user input and quit
  return ()