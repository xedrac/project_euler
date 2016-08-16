module Main where

import Data.List

limit = 1000
mult3 = takeWhile (<limit) [3,6..]
mult5 = takeWhile (<limit) [5,10..]
mult3or5unique = nub $ sort (mult3 ++ mult5)

main = print $ sum mult3or5unique
