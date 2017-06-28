
module Main (main) where

import Lib
import Web.Scotty

main :: IO ()
main = scotty 3000 routes
