{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module Noroshi.Cmd.Options where

import           RIO

import           Data.Extensible
import           Data.Extensible.GetOpt
import           Noroshi.Config         (defaultConfig)

type Options = Record
  '[ "input"   >: [String]
   , "version" >: Bool
   , "verbose" >: Bool
   , "config"  >: FilePath
   , "dry_run" >: Bool
   ]

versionOpt :: OptDescr' Bool
versionOpt = optFlag [] ["version"] "Show version"

verboseOpt :: OptDescr' Bool
verboseOpt = optFlag ['v'] ["verbose"] "Enable verbose mode: verbosity level \"debug\""

configOpt :: OptDescr' FilePath
configOpt = optLastArgWithDefault ['c'] ["config"] path "CONFIG-YAML" "Use specify config yaml path"
  where
    path = defaultConfig ^. #root <> "/config.yaml"

dryrunOpt :: OptDescr' Bool
dryrunOpt = optFlag [] ["dry_run"] "Verify to could read bases and config"

optLastArgWithDefault
  :: [Char]   -- ^ short option
  -> [String] -- ^ long option
  -> String   -- ^ default value
  -> String   -- ^ placeholder
  -> String   -- ^ explanation
  -> OptDescr' String
optLastArgWithDefault ss ls dv ph expl = fromMaybe dv <$> optLastArg ss ls ph expl
