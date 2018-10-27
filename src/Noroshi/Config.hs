{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module Noroshi.Config where

import           RIO

import           Data.Extensible
import qualified Data.Yaml               as Y
import           Noroshi.Data.ConfigInfo (ConfigInfo)

type Config = Record
  '[ "bases"   >: [ConfigInfo]
   , "configs" >: [ConfigInfo]
   ]

readConfig :: (MonadIO m, MonadThrow m) => FilePath -> m Config
readConfig = Y.decodeFileThrow
