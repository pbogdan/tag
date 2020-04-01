module Main where

import           Protolude

import           Tag

main :: IO ()
main = do
  command <- processOptions
  case command of
    Read   format  paths -> read format paths
    Write  updates path  -> write updates path
    Rename format  path  -> rename format path
