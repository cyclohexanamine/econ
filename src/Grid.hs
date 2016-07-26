module Grid where

import Types
import Loader
import Core


-- DEBUG

iterN :: Float -> Int -> World -> World
iterN t 0 w = w
iterN t n w = updateWorld t . iterN t (n-1) $ w

fromR = \(Right x) -> x

lW :: IO (World)
lW = fmap fromR . loadConfFile $ "in.txt"

firstSq = (!!0) . worldSquares

getGL = map fst . sqPrices . firstSq