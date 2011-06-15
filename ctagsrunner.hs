module Main where
import System.Process (proc, createProcess)
import System.INotify (withINotify, Event(..), EventVariety(..), addWatch)
import System.Directory (getDirectoryContents, doesDirectoryExist, doesFileExist)
import System.FilePath ((</>), takeExtension)
import qualified Control.Monad.Stream as M
import qualified Data.List.Stream as L
import qualified Data.Set as S
import Control.Concurrent
import Control.Concurrent.STM
import System.IO.Unsafe(unsafePerformIO)
import Control.Monad (when)
import qualified Text.PrettyPrint.Leijen as PP

running = unsafePerformIO $ newTVarIO False

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
hasktags path = proc "hasktags" (["-c", path])

getCtags :: FilePath -> Event -> Maybe (IO ())
getCtags root (Modified _ path) = path >>= router root
getCtags root (Deleted _ path ) = router root path
getCtags _ _ = Nothing

filetypes ::  [[Char]]
filetypes = [".php", ".js", ".c", ".C", ".cpp"]
hasktypes = [".hs", ".lhs"]

router :: FilePath -> FilePath -> Maybe (IO ())
router root path | "tags" `L.isSuffixOf` path = Nothing
                 | takeExtension path `elem` filetypes = return $ defer root path basic
                 | takeExtension path `elem` hasktypes = return $ defer root path hask
                 | otherwise = Nothing

defer :: FilePath -> FilePath -> (FilePath -> IO ()) -> IO ()
defer root path f = do
  let path' = root </> path
  doesFileExist path' >>= \e -> when e $ do
    waitForCompletion
    atomically $ writeTVar running True
    putStrLn "Running ctags"
    f path'
    atomically $ writeTVar running False
  where
    waitForCompletion = atomically $ do
      r <- readTVar running
      if not r
         then return ()
         else retry

basic :: FilePath -> IO ()
basic path = createProcess (ctags path) >> return ()

hask :: FilePath -> IO ()
hask path = createProcess (hasktags path) >> return ()

inotify ::  FilePath -> Event -> IO ()
inotify root event = maybe (return ()) id $ getCtags root event

main ::  IO ()
main = withINotify $ \i -> do
  dirs <- getRecursiveContents "."
  let msg' = "Following changes in these directories:"
      msg  = (PP.nest 1 $ L.foldr (PP.<$>) PP.empty $ L.map PP.text (msg' : dirs))
  print msg
  M.mapM_ (\f -> addWatch i [Modify, Delete] f (inotify f)) ("." : dirs)
  getLine -- Wait for user input and quit
  return ()
