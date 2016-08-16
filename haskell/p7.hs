module Main where

-- Find the 10001st prime number


-- Returns true if a is a prime number, and ps is a list of all primes prior to a
isPrime :: Integer -> [Integer] -> Bool
isPrime 2 _  = True
isPrime _ [] = False  -- Must pass in the list of prior primes!
isPrime n ps
    | p > sqrtn    = True
    | rem n p == 0 = False
    | otherwise    = isPrime n (tail ps)
    where
        p     = head ps
        sqrtn = (toInteger . ceiling . sqrt . fromIntegral) n


-- Recursively find the next prime based on the list of known primes in ps.  n is the current
-- candidate number being tested.
nextPrime' :: Integer -> [Integer] -> Integer
nextPrime' n ps
    | n <= p       = nextPrime' (p+1) ps  -- a must be larger than the last known prime
    | isPrime n ps = n
    | otherwise    = nextPrime' (n+1) ps
    where
        p = last ps


-- Helper to make the interface simple
nextPrime :: [Integer] -> Integer
nextPrime [] = 2 
nextPrime ps = nextPrime' (last ps) ps


-- Calculate the nth prime
calcNthPrime' :: Int -> [Integer] -> Integer
calcNthPrime' n [] = calcNthPrime' n [2,3,5,7]
calcNthPrime' n ps
    | n < 1           = 0
    | n <= length ps  = ps !! (n-1)
    | otherwise       = calcNthPrime' n (ps ++ ((nextPrime ps):[]))


calcNthPrime n = calcNthPrime' n []


main = do
    let answer = calcNthPrime 10001
    putStr "The answer is: "
    print answer
