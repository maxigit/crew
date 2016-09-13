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

--  find path in the current directory or above
findUp :: FilePath -> FilePath -> IO [FilePath]
findUp path cwd  = fmap catMaybes $ mapM exists (reverse $ inits (splitDirectories (normalise cwd)))
  where exists dirs = do
          let path' = joinPath (dirs ++ [path])
          found <- doesFileExist path'
          return $ if found
                   then Just path'
                   else Nothing

data StringOperation = Replace String | Concat (Maybe  String) (Maybe String) deriving (Read, Show )
instance FromJSON StringOperation where
  parseJSON (String v) = return $ Replace (unpack v)
  parseJSON (Object v) = do
    prepend <- v .:? "prepend"
    append <- v .:? "append"
    return $ Concat (fmap unpack prepend) (fmap unpack append)
  parseJSON v = error $ "Invalid Yaml " ++ show v

data Config = Config { progName :: String -- ^ program name 
                     , subConf :: SubConfig
                     } deriving (Read, Show)

data SubConfig = SubConfig
  { cmd :: Maybe String -- ^ command to substitue with
  , subcommands :: [(String, String)]
  , translations :: [(String, String)]
  , message :: Maybe String -- ^ to display before launching the command
  , environments :: [(String, StringOperation)]
  } deriving (Read, Show)

-- subcommands = undefined
-- translations = undefined

instance FromJSON SubConfig where
  parseJSON  (Object v) = do
    cmd <- v .:? "cmd"
    subcommands <- objectToPairs unpack' =<< v .:? "sub"
    translations <- objectToPairs unpack' =<< v .:? "args"
    message <- v .:? "msg"
    environments <- objectToPairs parseJSON =<< v .:? "env"

    return $ SubConfig cmd
                      subcommands
                      translations
                      message
                      environments
    where objectToPairs _ Nothing = return []
          objectToPairs f (Just (Object cs)) = mapM (unpackPair f) (H.toList cs)
          objectToPairs _ val = error $ "Invalid Yaml object " ++ show val
          unpackPair f (key, v') = do
            val <- f v'
            return (unpack key, val)

          unpack' (String v') = return $ unpack v'
          unpack' Null = return $ ""
          unpack' o = error $ "Invalid Yaml " ++ show o

  parseJSON  v = error $ "Invalid Yaml" ++ show v

          

        
    
    
  

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
          sub <- (H.lookup command configs)
          return $ Config command sub

-- | Split a string on space, unless
-- there spaces are escaped
splitOnSpace :: String -> [String]
splitOnSpace xs = go [] [] xs where
  go result current [] = result ++ [current]
  go result current ('\\':' ':xs) = go result (current++" ") xs
  go result current (' ':xs) = go (result ++ [current]) [] xs
  go result current (x:xs) = go result (current++[x]) xs

  
translate :: Config -> String -> [String] -> (String, [String])
translate config command args = let
  subc = subConf config
  (command:extra) = fromMaybe [progName config] (splitOnSpace `fmap` cmd subc)
  (sub, args') = translateSubcommand subc args
  in (command, extra ++ sub  ++ (concatMap (translateArgument subc) args'))

-- | only translate the full word ending to an ==
translateArgument :: SubConfig -> String -> [String]
translateArgument config cs =
  let (arg, value) = span (/= '=') cs
  in case lookup arg (translations config)  of
       Nothing -> [cs]
       Just "" -> []
       Just new -> splitOnSpace new ++ [value]

-- | check if the first argument is a subcommand and translate it
translateSubcommand :: SubConfig -> [String] -> ([String], [String])
translateSubcommand _ [] = ([], [])
translateSubcommand config all@(sub:args) =
  case lookup sub (subcommands config) of
       Nothing -> ([], all)
       Just new -> (splitOnSpace new, args)
  
execute :: String -> [String] -> IO ()
execute command args = callProcess command args

setEnvironment :: SubConfig -> IO ()
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
  case configM of
    Nothing -> error $ "No configuration found for command " ++ command
    Just config -> do
      let (command', args') = translate config command args
      setEnvironment (subConf config)
      traverse putStrLn (message (subConf config))
      execute command' args'
