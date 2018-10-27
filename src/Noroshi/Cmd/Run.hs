{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module Noroshi.Cmd.Run where

import           RIO
import           RIO.Directory
import           RIO.FilePath

import           Data.Aeson              (Value (..))
import           Data.Extensible
import qualified Data.Yaml               as Y
import           Noroshi.Cmd.Options
import           Noroshi.Config          (readConfig)
import           Noroshi.Data.ConfigInfo
import           Noroshi.Data.ConfigType (generateConfig, readBaseConfigs)
import           Noroshi.Env


run :: (MonadUnliftIO m, MonadThrow m) => Options -> m ()
run opts = do
  logOpts <- logOptionsHandle stdout (opts ^. #verbose)
  config  <- readConfig ".noroshi/config.yaml"
  withLogFunc logOpts $ \logger -> do
    let env = #logger @= logger
           <: #config @= config
           <: nil
    runRIO env run'

run' :: RIO Env ()
run' = do
  conf  <- asks (view #config)
  bases <- readBaseConfigs (conf ^. #bases)
  forM_ (conf ^. #configs) $ \info -> do
    tpl <- readConfigTemplate info
    case generateConfig bases tpl of
      Left e  -> logError $ display ("fail to generate config: " <> e)
      Right v -> writeConfig info v

readConfigTemplate :: ConfigInfo -> RIO Env Value
readConfigTemplate info = do
  let path = getYamlPath info
  logDebug $ fromString ("read config template: " <> path)
  Y.decodeFileThrow path

writeConfig :: ConfigInfo -> Value -> RIO Env ()
writeConfig info conf = do
  let path = getOutputPath info
  createDirectoryIfMissing True (dropFileName path)
  logDebug $ fromString ("write config: " <> path)
  liftIO $ Y.encodeFile path conf

showNotImpl :: MonadIO m => m ()
showNotImpl = hPutBuilder stdout "not yet implement command."
