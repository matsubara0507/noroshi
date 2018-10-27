{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module Noroshi.Data.ConfigType where

import           RIO
import qualified RIO.HashMap             as HM
import qualified RIO.List                as L
import qualified RIO.Text                as Text
import qualified RIO.Vector              as V

import           Data.Aeson              (Value (..))
import           Data.Foldable           (foldlM)
import qualified Data.Yaml               as Y
import           Noroshi.Data.ConfigInfo (ConfigInfo, ConfigTemplate,
                                          getYamlPath)
import           Noroshi.Utils           (maybeToEither)

type BaseConfig = Value

readBaseConfigs :: MonadIO m => [ConfigInfo] -> m (HashMap Text BaseConfig)
readBaseConfigs = fmap HM.fromList . mapM readBaseConfigWithKey

readBaseConfigWithKey :: MonadIO m => ConfigInfo -> m (Text, BaseConfig)
readBaseConfigWithKey info = do
  conf <- Y.decodeFileThrow $ getYamlPath info
  pure (info ^. #name, conf)

generateConfig :: HashMap Text BaseConfig -> ConfigTemplate -> Either Text Value
generateConfig bases = \case
  (Object hash) -> Object <$> traverse (generateConfig bases) hash
  (Array arr)   -> Array <$> V.mapM (generateConfig bases) arr
  (String txt)  -> lookupWith bases =<< parseKey txt
  val           -> Left $ tshow val

lookupWith :: HashMap Text BaseConfig -> [Text] -> Either Text Value
lookupWith bases keys = do
  key  <- maybeToEither "keys cannot be empty" $ L.headMaybe keys
  conf <- maybeToEither (err key) $ HM.lookup key bases
  lookupJSON (drop 1 keys) conf
  where
    err k = mconcat [k, " of ", Text.intercalate "." keys ," is not found"]

lookupJSON :: [Text] -> Value -> Either Text Value
lookupJSON keys json = foldlM lookupJSON' json keys
  where
    lookupJSON' (Object h) key = maybeToEither (err key) $ HM.lookup key h
    lookupJSON' _ key          = Left (err key)
    err k = mconcat [k, " of ", Text.intercalate "." keys ," is not found"]

parseKey :: Text -> Either Text [Text]
parseKey = pure . Text.split (== '.')
