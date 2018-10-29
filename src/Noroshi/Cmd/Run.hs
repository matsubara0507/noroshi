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
import           Noroshi.Config
import           Noroshi.Data.ConfigInfo
import           Noroshi.Data.ConfigType (generateConfig, readBaseConfigs)
import           Noroshi.Env


run :: (MonadUnliftIO m, MonadThrow m) => Options -> m ()
run opts = do
  let path = opts ^. #config
  logOpts <- logOptionsHandle stdout (opts ^. #verbose)
  config  <- readConfig path
  withLogFunc logOpts $ \logger -> do
    let env = #logger @= logger
           <: #config @= config
           <: #dry_run @= (opts ^. #dry_run)
           <: nil
    runRIO env $ do
      whenM (not <$> doesFileExist path) $ do
        logError (fromString $ "config is not found: " <> path)
        logWarn (fromString $ "use default config: " <> show defaultConfig)
      run'

run' :: RIO Env ()
run' = do
  conf  <- asks (view #config)
  bases <- readBaseConfigs (getConfigsPath conf) (conf ^. #bases)
  forM_ bases $ \base ->
    logDebug (fromString $ "read base config: " <> show base)
  forM_ (conf ^. #configs) $ \info -> do
    tpl <- readConfigTemplate info
    case generateConfig bases tpl of
      Left e  -> logError $ display ("fail to generate config: " <> e)
      Right v -> writeConfig info v

readConfigTemplate :: ConfigInfo -> RIO Env Value
readConfigTemplate info = do
  root <- asks (getConfigsPath . view #config)
  let path = getYamlPath root info
  logDebug $ fromString ("read config template: " <> path)
  Y.decodeFileThrow path

writeConfig :: ConfigInfo -> Value -> RIO Env ()
writeConfig info conf = do
  root <- asks (getOutputsPath . view #config)
  let path = getOutputPath root info
  createDirectoryIfMissing True (dropFileName path)
  dryrun <- asks (view #dry_run)
  if dryrun then do
    logInfo  $ fromString ("write config (dry_run): " <> path)
    logDebug $ fromString ("writen config (dry_run): " <> show conf)
  else do
    logDebug $ fromString ("write config: " <> path)
    liftIO $ Y.encodeFile path conf

showNotImpl :: MonadIO m => m ()
showNotImpl = hPutBuilder stdout "not yet implement command."
