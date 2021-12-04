module Main where

import UI(gameInit)
import Core


main :: IO ()
main =
  do
    g <- gameInit
    return ()