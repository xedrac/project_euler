module Main where

-- Return a list of prime factors for the number n
factor n 0 _ = factor n 2 [] 
factor n 1 _ = factor n 2 []
factor n k xs
    | rem n k == 0 = factor (div n k) 2 (k : xs)  -- found a prime factor, continue factoring the quotient
    | n > k        = factor n (k+1) xs
    | otherwise    = xs

main = do
    let xs = factor 600851475143 2 []
    let answer = if null xs
        then "There are no prime factors"
        else "The largest prime factor is: " ++ show (head xs)
    print xs
    putStrLn answer
