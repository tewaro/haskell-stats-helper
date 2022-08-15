module Lib
    ( ave,
      var,
      std,
      showStdDev,
      showAve,
      showVar
    ) where

import Data.Ratio
import Data.Number.CReal

(.>) = flip (.)

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
std = var .> fromRational .> sqrt

showRat     :: Int -> Rational -> String
showRat digits    = fromRational .> showCReal digits

showStdDev  :: Int -> [Int] -> String
showStdDev digits = std .> showCReal digits

showVar     :: Int -> [Int] -> String
showVar digits    = var .> showRat digits

showAve     :: Int -> [Int] -> String
showAve digits    = ave .> showRat digits
