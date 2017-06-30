
module Main (main) where

import Lib
import Web.Scotty
import Data.Time.Clock

integralTime :: (Integral a) => IO a
integralTime = fmap (floor . utctDayTime) getCurrentTime

main :: IO ()
main = do
  seed <- integralTime
  scotty 3000 (routes seed)
