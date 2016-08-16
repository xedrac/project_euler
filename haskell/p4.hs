module Main where

import System.Environment
import Data.List
import Data.Ord

isPalindrome :: (Show a, Read a, Eq a) => a -> Bool
isPalindrome n = (n == read (reverse $ show n))

threeDigitPairs a xs
    | a < 100   = xs
    | a > 999   = threeDigitPairs 999 []
    | otherwise = zipWith (\x y -> (x,y)) (repeat a) [a,a-1..100] ++ threeDigitPairs (a-1) xs

products = map (\(x,y) -> x * y) $ threeDigitPairs 999 []

sortedProducts = sortBy (flip compare) products

largestPalindrome = head $ dropWhile (not . isPalindrome) sortedProducts

main :: IO ()
main = do
    print largestPalindrome
