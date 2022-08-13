module Lib
    ( ave,
      var,
      std,
      showStdDev,
      showAve
    ) where

import Data.Ratio
import Data.Number.CReal


ave :: [Int] -> Rational
ave [] = 0
ave x  = toInteger (sum x) % toInteger (length x)

varHelper :: Rational -> Rational -> Int -> [Int] -> Rational
varHelper _ _ 0   _       = 0
varHelper _ r len []      = r / toRational len
varHelper a r len (x:xs)  = varHelper a (r + (toRational x - a) * (toRational x - a)) len xs

var :: [Int] -> Rational
var x = varHelper (ave x) 0 (length x) x

std :: [Int] -> CReal
std = sqrt . fromRational . var

showRat     :: Int -> Rational -> String
showRat digits    = showCReal digits . fromRational

showStdDev  :: Int -> [Int] -> String
showStdDev digits = showCReal digits . std

showVar     :: Int -> [Int] -> String
showVar digits    = showRat digits . var

showAve     :: Int -> [Int] -> String
showAve digits    = showRat digits . ave
