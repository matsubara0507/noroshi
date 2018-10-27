{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module Noroshi.Data.ConfigInfo where

import           RIO
import qualified RIO.Text        as Text

import           Data.Aeson      (Value)
import           Data.Extensible
import           Noroshi.Utils   (validateWith)

type ConfigInfo = Record
  '[ "name"   >: Text
   , "path"   >: Maybe FilePath
   , "yaml"   >: Maybe Text
   , "github" >: Maybe Text
   ]

type ConfigTemplate = Value

getGitHubOwner :: ConfigInfo -> Maybe Text
getGitHubOwner = fmap fst . getGitHubOwnerAndRepo

getGitHubRepo :: ConfigInfo -> Maybe Text
getGitHubRepo = fmap snd . getGitHubOwnerAndRepo

getGitHubOwnerAndRepo :: ConfigInfo -> Maybe (Text, Text)
getGitHubOwnerAndRepo info = do
  res <- fmap (Text.drop 1) . Text.break (== '/') <$> (info ^. #github)
  validateWith validator res
  where
    validator (owner, repo) = not (Text.null owner || Text.null repo)

getYamlPath :: ConfigInfo -> FilePath
getYamlPath info =
  fromMaybe ".noroshi/configs" (info ^. #path) <> "/" <> getYamlName info

getOutputPath :: ConfigInfo -> FilePath
getOutputPath info = mconcat
  [ fromMaybe ".noroshi/out" (info ^. #path), "/"
  , fromMaybe "" (Text.unpack . (<> "/") <$> info ^. #github)
  , getYamlName info
  ]

getYamlName :: ConfigInfo -> FilePath
getYamlName info =
  Text.unpack $ fromMaybe (info ^. #name <> ".yaml") (info ^. #yaml)