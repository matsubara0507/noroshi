{-# LANGUAGE OverloadedLabels #-}

module Noroshi.Cmd
    ( module X
    , Cmd (..)
    , toCmd
    ) where

import           RIO

import           Noroshi.Cmd.Options as X
import           Noroshi.Cmd.Run     as X

data Cmd
  = PrintVersion
  | RunCmd Options
  deriving (Show, Eq)

toCmd :: Options -> Cmd
toCmd opts
  | opts ^. #version = PrintVersion
  | otherwise        = RunCmd opts
