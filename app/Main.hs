module Main where

import           Protolude

import           Tag

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["r", path]                -> read path
    ["w", path, artist]        -> write path (toS artist) Nothing
    ["w", path, artist, title] -> write path (toS artist) (Just . toS $ title)
    _                          -> do
      putText "Invalid arguments"
      exitFailure
