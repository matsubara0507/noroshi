{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeOperators    #-}

module Noroshi.Config where

import           RIO
import           RIO.Directory           (doesFileExist)

import           Data.Extensible
import qualified Data.Yaml               as Y
import qualified Data.Yaml.TH            as YTH
import           Instances.TH.Lift       ()
import           Noroshi.Data.ConfigInfo (ConfigInfo)

type Config = Record
  '[ "bases"   >: [ConfigInfo]
   , "configs" >: [ConfigInfo]
   ]

defaultConfig :: Config
defaultConfig = $$(YTH.decodeFile "template/config.yaml")

readConfig :: (MonadIO m, MonadThrow m) => FilePath -> m Config
readConfig = readConfigWith defaultConfig

readConfigWith :: (MonadIO m, MonadThrow m) => Config -> FilePath -> m Config
readConfigWith def path = do
  file <- readFileBinaryWith "" path
  case Y.decodeEither' file of
    Right Y.Null -> pure def
    _ -> do
      config <- either throwM pure $ Y.decodeEither' file
      pure $ constructWith def config

readFileBinaryWith :: MonadIO m => ByteString -> FilePath -> m ByteString
readFileBinaryWith def path =
  doesFileExist path >>= bool (pure def) (readFileBinary path)

constructWith :: RecordOf h xs -> Nullable (Field h) :* xs -> RecordOf h xs
constructWith def =
  hmapWithIndex $ \m x -> fromMaybe (hlookup m def) (getNullable x)
