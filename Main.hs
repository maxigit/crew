{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Maybe (maybeToList, fromMaybe, catMaybes)
import Data.Yaml.Config (loadYamlSettings, useEnv)
import System.Environment (getArgs, getProgName)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath (splitDirectories, joinPath, normalise)
import System.Process (callProcess)
import GHC.Generics
import Data.Aeson
import qualified Data.HashMap.Strict as H
import Data.Text (unpack)
import Data.Traversable (traverse)
import Data.List (inits)

--  find path in the current directory or above
findUp :: FilePath -> FilePath -> IO [FilePath]
findUp path cwd  = fmap catMaybes $ mapM exists (reverse $ inits (splitDirectories (normalise cwd)))
  where exists dirs = do
          let path' = joinPath (dirs ++ [path])
          found <- doesFileExist path'
          return $ if found
                   then Just path'
                   else Nothing

data Config = Config { progName :: String -- ^ program name 
                     , subConf :: SubConfig
                     } deriving (Read, Show, Generic)
data SubConfig = SubConfig
  { cmd :: Maybe String -- ^ command to substitue with
  , subcommands :: [(String, String)]
  , translations :: [(String, String)]
  , message :: Maybe String -- ^ to display before launching the command
  } deriving (Read, Show, Generic)

-- subcommands = undefined
-- translations = undefined

instance FromJSON SubConfig where
  parseJSON  (Object v) = do
    cmd <- v .:? "cmd"
    subcommands <- v .:? "sub"
    translations <- v .:? "args"
    message <- v .:? "msg"

    return $ SubConfig cmd (split subcommands) (split translations) message
    where split Nothing = []
          -- split x = [("json", show x)]
          split (Just (Object cs)) = map parseSub (H.toList cs)
          parseSub (key, String v) = (unpack key , unpack v)

        
    
    
  

-- ^ find an load the configuration for the given command
-- lookup for the ".cws" file in current file or up (recursively)
-- and find the corresponding entry
loadConfig :: String -> IO (Maybe Config)
loadConfig command = do
    cwd <- getCurrentDirectory
    paths <- findUp ".csw" cwd
    case paths of
      [] -> return Nothing
      _ -> do
        -- sub <- loadYamlSettings [".csw"] [] useEnv
        -- return . Just $ Config command sub
        configs <- loadYamlSettings paths [] useEnv
        return $ do
          sub <- (H.lookup command configs)
          return $ Config command sub

  
translate :: Config -> String -> [String] -> (String, [String])
translate config command args = let
  subc = subConf config
  command = fromMaybe (progName config) (cmd subc)
  (sub, args') = translateSubcommand subc args
  in (command, maybeToList sub  ++ map (translateArgument subc) args')

-- ^ only translate the full word ending to an ==
translateArgument :: SubConfig -> String -> String
translateArgument config cs =
  let (arg, value) = span (/= '=') cs
  in case lookup arg (translations config)  of
       Nothing -> cs
       Just new -> new ++ value

  -- ^ check if the first argument is a subcommand and translate it
translateSubcommand :: SubConfig -> [String] -> (Maybe String, [String])
translateSubcommand _ [] = (Nothing, [])
translateSubcommand config all@(sub:args) =
  case lookup sub (subcommands config) of
       Nothing -> (Nothing, all)
       Just new -> (Just new, args)
  
execute :: String -> [String] -> IO ()
execute command args = callProcess command args

main :: IO ()
main = do
  command <- getProgName
  args <- getArgs


  configM <- loadConfig command
  case configM of
    Nothing -> error $ "No configuration found for command " ++ command
    Just config -> do
      let (command', args') = translate config command args
      traverse putStrLn (message (subConf config))
      execute command' args'
