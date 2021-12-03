module Main where

import UI(gameInit)
import Parachute


main :: IO ()
main =
  do
    g <- gameInit
    return ()