module Main where
import System.Environment (getArgs, getProgName)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath (splitDirectories, joinPath, normalise)

--  find path in the current directory or above
findUp :: FilePath -> FilePath -> IO (Maybe FilePath)
findUp path cwd  = go (splitDirectories (normalise cwd))
  where go dirs = do 
          let path' = joinPath (dirs ++ [path])
          found <- doesFileExist path'
          case (found, dirs) of
            (True, _) -> return (Just path')
            (False, []) -> return Nothing
            (False, _) -> go (init dirs)

main :: IO ()
main = do
  arg0 <- getProgName
  args <- getArgs

  configPath <- findUp ".csw" =<< getCurrentDirectory
  print configPath
  print arg0
  
