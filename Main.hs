{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Maybe (maybeToList, fromMaybe, catMaybes)
import Data.Yaml.Config (loadYamlSettings, useEnv)
import System.Environment (getArgs, getProgName, setEnv, lookupEnv)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath (splitDirectories, joinPath, normalise)
import System.Process (callProcess)
import GHC.Generics
import Data.Aeson
import qualified Data.HashMap.Strict as H
import Data.Text (unpack)
import Data.Traversable (traverse)
import Data.List (inits)

-- | find path in the current directory or above
findUp :: FilePath -> FilePath -> IO [FilePath]
findUp path cwd  = fmap catMaybes $ mapM exists (reverse $ inits (splitDirectories (normalise cwd)))
  where exists dirs = do
          let path' = joinPath (dirs ++ [path])
          found <- doesFileExist path'
          return $ if found
                   then Just path'
                   else Nothing

-- * Configuration
-- | Operation on String
data StringOperation = Replace String | Concat (Maybe  String) (Maybe String) deriving (Read, Show, Eq)

-- | Top level configuration
data Config = Config { progName :: String -- ^ program name 
                     , transConfig :: TransConfig
                     , subcommands :: [(String, SubConfig)]
                     } deriving (Read, Show)

-- For JSON only
data ConfigJ = ConfigJ TransConfig [(String, SubConfig)]

-- | Sub command configuration
data SubConfig = SubConfig
  { subCommand :: Maybe String
  , subTransConfig :: TransConfig
  } deriving (Read, Show)

-- | transformation configuration. Can relate to a commmand (ex stac,) or a subcommand (stack build)
data TransConfig = TransConfig
  { cmd :: Maybe String -- ^ command to substitue with
  , translations :: [(String, String)]
  , message :: Maybe String -- ^ to display before launching the command
  , environments :: [(String, StringOperation)]
  } deriving (Read, Show)

emptyTransConfig = TransConfig Nothing [] Nothing []

-- * From Json 

invalidYamlObject v = error $ "Invalid configuration :" ++ show v
objectToPairs _ Nothing = return []
objectToPairs f (Just (Object cs)) = mapM (unpackPair f) (H.toList cs)
objectToPairs _ val = invalidYamlObject val
unpackPair f (key, v') = do
  val <- f v'
  return (unpack key, val)

unpack' (String v') = return $ unpack v'
unpack' Null = return $ ""
unpack' o = invalidYamlObject o

instance FromJSON StringOperation where
  parseJSON (String v) = return $ Replace (unpack v)
  parseJSON (Object v) = do
    prepend <- v .:? "prepend"
    append <- v .:? "append"
    return $ Concat (fmap unpack prepend) (fmap unpack append)
  parseJSON v = invalidYamlObject v

instance FromJSON TransConfig where
  parseJSON Null = return emptyTransConfig
  parseJSON  (Object v) = do
    cmd <- v .:? "cmd"
    translations <- objectToPairs unpack' =<< v .:? "args"
    message <- v .:? "msg"
    environments <- objectToPairs parseJSON =<< v .:? "env"

    return $ TransConfig cmd
                         translations
                         message
                         environments

  parseJSON  v = invalidYamlObject v

instance FromJSON SubConfig where
  parseJSON (String subCmd) = return $ SubConfig (Just $ unpack subCmd) emptyTransConfig
  parseJSON  o@(Object v) = do
    subCommandName <- v .:? "subcmd"
    trans <- parseJSON o

    return $ SubConfig (fmap unpack subCommandName) trans

  parseJSON  v = invalidYamlObject v

instance FromJSON ConfigJ where
  parseJSON o@(Object v) = do
    subs <- objectToPairs parseJSON =<< v .:? "sub"
    trans <- parseJSON o

    return $ ConfigJ trans subs
    
  parseJSON  v = invalidYamlObject v

        
    
    
  

-- | find an load the configuration for the given command
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
          ConfigJ trans sub <- (H.lookup command configs)
          return $ Config command trans sub

-- | Split a string on space, unless
-- there spaces are escaped
splitOnSpace :: String -> [String]
splitOnSpace xs = go [] [] xs where
  go result current [] = result ++ [current]
  go result current ('\\':' ':xs) = go result (current++" ") xs
  go result current (' ':xs) = go (result ++ [current]) [] xs
  go result current (x:xs) = go result (current++[x]) xs

  
translate :: Config -> String -> [String] -> (String, [String], TransConfig)
-- current transConfig can be overriden by subcommand one
-- we need first to try to find if the first argument is a registered subcommand
translate config command args@(subCmd:_) | Just subConf <- lookup subCmd (subcommands config) =
  let globalTrans = transConfig config
      subTrans = subTransConfig subConf
      merge f | f subTrans == Nothing  = f globalTrans
      merge f = f subTrans
      mergeL f | f subTrans == []  = f globalTrans
      mergeL f = f subTrans
      trans = TransConfig (merge cmd) (mergeL translations) (merge message) (mergeL environments)

  in translate' config { transConfig = trans } command args

-- | Can probably be rewritten to only use TransConfig but we are using
-- the old code
translate config command args = translate' (config) command args 

translate' config command args = let
  subc = transConfig config
  (command:extra) = maybe [progName config] splitOnSpace (cmd subc)
  (sub, args') = translateSubcommand config args
  in ( command
     , extra ++ sub  ++ concatMap (translateArgument subc) args'
     , subc)

-- | only translate the full word ending to an ==
translateArgument :: TransConfig -> String -> [String]
translateArgument config cs =
  let (arg, value) = span (/= '=') cs
  in case lookup arg (translations config)  of
       Nothing -> [cs]
       Just "" -> []
       Just new -> splitOnSpace new ++ [value]

-- | check if the first argument is a subcommand and translate it
translateSubcommand :: Config -> [String] -> ([String], [String])
translateSubcommand _ [] = ([], [])
translateSubcommand config all@(sub:args) =
  case lookup sub (subcommands config) of
       Just subC | Just new <- subCommand subC  -> (splitOnSpace new, args)
       Nothing -> ([], all)
  
execute :: String -> [String] -> IO ()
execute command args = callProcess command args

setEnvironment :: TransConfig -> IO ()
setEnvironment config = mapM_ set  (environments config) where
  set (var, op) = do
    new <- case op of
      Replace value -> return value
      Concat prepend append -> do
        value <- lookupEnv var
        return . concat $ catMaybes [prepend , value , append] 
      
    -- print (var, new)
    setEnv var new
main :: IO ()
main = do
  command <- getProgName
  args <- getArgs


  configM <- loadConfig command
  -- print configM
  case configM of
    Nothing -> error $ "No configuration found for command " ++ command
    Just config -> do
      let (command', args', transc) = translate config command args
      -- print command'
      -- print args'
      -- print transc
      setEnvironment transc
      _ <- traverse putStrLn (message transc)
      execute command' args'
