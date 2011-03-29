module Main where
import System.Process (proc, createProcess)
import System.INotify (withINotify, Event(..), EventVariety(..), addWatch)
import System.Directory (getDirectoryContents, doesDirectoryExist, doesFileExist)
import System.FilePath ((</>), takeExtension)
import qualified Control.Monad.Stream as M
import qualified Data.List.Stream as L
import Data.Monoid(mempty)
import qualified Data.Set as S
import Data.Set (Set(..))
import Control.Concurrent.MVar
import System.IO.Unsafe(unsafePerformIO)

paths = unsafePerformIO $ newMVar S.empty

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
getPath (Deleted _ path ) = acceptable path
getPath _ = mempty -- I think this is QOverFlow/DeletedSelf or maybe even Unknown

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
	    run (root </> path)
	    return ()
  where
    run path = do
      exists <- doesFileExist path
      if exists then modifyhandler path
		else deletehandler path
    deletehandler path = withMVar paths $ \set -> do
      print "deletion handler"
      let newset = S.delete path set
      mapM_ (createProcess . ctags) $ S.toList newset
      return newset
    modifyhandler path = withMVar paths $ \set -> do
      print "modify handler"
      createProcess $ ctags path
      return $ S.insert path set

main ::  IO ()
main = withINotify $ \i -> do
  dirs <- getRecursiveContents "."
  print dirs
  M.mapM_ (\f -> addWatch i [Modify, Delete] f (inotify f)) ("." : dirs)
  getLine -- Wait for user input and quit
  return ()
