module Main where

import UI(gameInit)
import Parachuting


main :: IO ()
main =
  do
    g <- gameInit
    return ()