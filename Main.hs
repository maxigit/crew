{-# LANGUAGE DeriveGeneric #-}
module Main where

import Data.Maybe (maybeToList, fromMaybe)
import Data.Yaml.Config (loadYamlSettings, useEnv)
import System.Environment (getArgs, getProgName)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath (splitDirectories, joinPath, normalise)
import System.Process (callProcess)
import GHC.Generics
import Data.Aeson

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

data Config = Config { progName :: String -- ^ program name 
  , cmd :: Maybe String -- ^ command to substitue with
  , subcommands :: [(String, String)]
  , translations :: [(String, String)]
  } deriving (Generic)

instance FromJSON Config

-- ^ find an load the configuration for the given command
-- lookup for the ".cws" file in current file or up (recursively)
-- and find the corresponding entry
loadConfig :: String -> IO (Maybe Config)
loadConfig command = do
    cwd <- getCurrentDirectory
    pathM <- findUp command cwd
    case pathM of
      Nothing -> return Nothing
      Just path -> do
        configs <- loadYamlSettings [path] [] useEnv
        return (lookup command configs)

  
translate :: Config -> String -> [String] -> (String, [String])
translate config command args = let
  command = fromMaybe command (cmd config)
  (sub, args') = translateSubcommand config args
  in (command, maybeToList sub  ++ map (translateArgument config) args')

-- ^ only translate the full word ending to an ==
translateArgument :: Config -> String -> String
translateArgument config cs =
  let (arg, value) = span (/= '=') cs
  in case lookup arg (translations config)  of
       Nothing -> cs
       Just new -> new ++ value

  -- ^ check if the first argument is a subcommand and translate it
translateSubcommand :: Config -> [String] -> (Maybe String, [String])
translateSubcommand _ [] = (Nothing, [])
translateSubcommand config all@(sub:args) =
  case lookup sub (subcommands config) of
       Nothing -> (Nothing, all)
       Just new -> (Just new, args)
  
execute :: String -> [String] -> IO ()
execute command args =  callProcess "env" [] -- command args

main :: IO ()
main = do
  command <- getProgName
  args <- getArgs


  configM <- loadConfig command
  case configM of
    Nothing -> error $ "No configuration found for command " ++ command
    Just config -> do
      let (command', args') = translate config command args
      execute command' args'
