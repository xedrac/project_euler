module Main where

-- The slow way
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 2
fib n = fib (n-1) + fib (n-2)
answer1 = sum $ filter even $ takeWhile (<4000000) $ map fib [1,2..]

-- Much faster way
fib2 :: Num a => [a]
fib2 = 1 : 2 : zipWith (+) fib2 (tail fib2)
answer2 = sum $ filter even $ takeWhile (<4000000) fib2

main = print answer2

