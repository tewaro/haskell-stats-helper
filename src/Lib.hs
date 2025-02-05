module Lib
    ( std,
      ave,
      var,
      min,
      q1,
      med,
      q3,
      max,
      showStdDev,
      showAve,
      showVar,
      showMin,
      showQ1,
      showMed,
      showQ3,
      showMax
    ) where

import Data.List
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

slice :: Int -> Int -> [Int] -> [Int]
slice start end = drop start .> take (end - start)

medHelper x = slice (len2 - mod2) (len2 + 1) x
  where
  len2 = (length x) `div` 2
  mod2 = ((length x) + 1) `mod` 2

med :: [Int] -> Rational
med = sort .> medHelper .> ave

q1Helper :: [Int] -> [Int]
q1Helper x = take (div (length x) 2) x

-- 0 1 2 | 3 4 5
-- 0 1 2 | 3 | 4 5 6
q1 :: [Int] -> Rational
q1 = sort .> q1Helper .> med

q3Helper x = drop (len2 + mod2) x
  where
  len2 = div (length x) 2
  mod2 = mod (length x) 2

q3 :: [Int] -> Rational
q3 = sort .> q3Helper .> med

showInt     :: Int -> Int -> String
showInt digits = fromIntegral .> showCReal digits

showRat     :: Int -> Rational -> String
showRat digits    = fromRational .> showCReal digits

showStdDev  :: Int -> [Int] -> String
showStdDev digits = std .> showCReal digits

showVar     :: Int -> [Int] -> String
showVar digits    = var .> showRat digits

showAve     :: Int -> [Int] -> String
showAve digits    = ave .> showRat digits

showMin     :: Int -> [Int] -> String
showMin digits    = minimum .> showInt digits

showQ1     :: Int -> [Int] -> String
showQ1 digits     = q1 .> showRat digits

showMed     :: Int -> [Int] -> String
showMed digits     = med .> showRat digits

showQ3     :: Int -> [Int] -> String
showQ3 digits     = q3 .> showRat digits

showMax     :: Int -> [Int] -> String
showMax digits    = maximum .> showInt digits
