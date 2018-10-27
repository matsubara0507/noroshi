module Noroshi.Utils where

import           RIO

validateWith :: (a -> Bool) -> a -> Maybe a
validateWith p x = if p x then Just x else Nothing

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e = maybe (Left e) pure
