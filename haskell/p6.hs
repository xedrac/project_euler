module Main where

sumOfSquares :: Num a => [a] -> a
sumOfSquares xs = sum $ map (^2) xs

squareOfSum :: Num a => [a] -> a
squareOfSum xs = (sum xs) ^ 2

main = do
    let values = [1..100]
    print $ squareOfSum values - sumOfSquares values
