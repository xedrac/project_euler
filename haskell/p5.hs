module Main where

divides :: Integral a => a -> a -> Bool
divides n x = (rem n x == 0)

dividesAll :: Integral a => a -> [a] -> Bool
dividesAll n [] = True
dividesAll n (x:xs)
    | divides n x = dividesAll n xs
    | otherwise   = False


candidates :: Integral a => [a]
candidates = [380,760..]  -- 20*19

d = takeWhile (== False) $ zipWith (dividesAll) candidates (repeat [1..20])

main = print $ (length d + 1) * 380

