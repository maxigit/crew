{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Maybe (maybeToList, fromMaybe, catMaybes)
import Data.Yaml.Config (loadYamlSettings, useEnv)
import System.Environment (getArgs, getProgName, setEnv)
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
  , environments :: [(String, String)]
  } deriving (Read, Show, Generic)

-- subcommands = undefined
-- translations = undefined

instance FromJSON SubConfig where
  parseJSON  (Object v) = do
    cmd <- v .:? "cmd"
    subcommands <- v .:? "sub"
    translations <- v .:? "args"
    message <- v .:? "msg"
    environments <- v .:? "env"

    return $ SubConfig cmd
                      (objectToPairs subcommands)
                      (objectToPairs translations)
                      message
                      (objectToPairs environments)
    where objectToPairs Nothing = []
          objectToPairs (Just (Object cs)) = map unpackPair (H.toList cs)
          objectToPairs val = error $ "Invalid Yaml object " ++ show val
          unpackPair (key, String v) = (unpack key , unpack v)
          unpackPair (key, Null) = (unpack key , "")

        
    
    
  

-- ^ find an load the configuration for the given command
-- lookup for the ".cws" file in current file or up (recursively)
-- and find the corresponding entry
loadConfig :: String -> IO (Maybe Config)
loadConfig command = do
    cwd <- getCurrentDirectory
    paths <- findUp ".crew" cwd
    case paths of
      [] -> return Nothing
      _ -> do
        configs <- loadYamlSettings paths [] useEnv
        return $ do
          sub <- (H.lookup command configs)
          return $ Config command sub

  
translate :: Config -> String -> [String] -> (String, [String])
translate config command args = let
  subc = subConf config
  command = fromMaybe (progName config) (cmd subc)
  (sub, args') = translateSubcommand subc args
  in (command, maybeToList sub  ++ catMaybes (map (translateArgument subc) args'))

-- ^ only translate the full word ending to an ==
translateArgument :: SubConfig -> String -> Maybe String
translateArgument config cs =
  let (arg, value) = span (/= '=') cs
  in case lookup arg (translations config)  of
       Nothing -> Just cs
       Just "" -> Nothing
       Just new -> Just (new ++ value)

  -- ^ check if the first argument is a subcommand and translate it
translateSubcommand :: SubConfig -> [String] -> (Maybe String, [String])
translateSubcommand _ [] = (Nothing, [])
translateSubcommand config all@(sub:args) =
  case lookup sub (subcommands config) of
       Nothing -> (Nothing, all)
       Just new -> (Just new, args)
  
execute :: String -> [String] -> IO ()
execute command args = callProcess command args

setEnvironment :: SubConfig -> IO ()
setEnvironment config = mapM_ (uncurry setEnv)  (environments config) where
  set (var, value) = do
    print (var, value)
    setEnv var value
main :: IO ()
main = do
  command <- getProgName
  args <- getArgs


  configM <- loadConfig command
  case configM of
    Nothing -> error $ "No configuration found for command " ++ command
    Just config -> do
      let (command', args') = translate config command args
      setEnvironment (subConf config)
      traverse putStrLn (message (subConf config))
      execute command' args'
