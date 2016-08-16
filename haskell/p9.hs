module Main where

pt = [ (a,b,c)| a <- [1..1000],
                b <- [1..1000],
                c <- [1..1000],
                a < b,
                b < c,
                a + b + c == 1000,
                a^2 + b^2 == c^2 ]

main = do
    let (a,b,c) = head pt
    let answer = a*b*c
    putStr "The triplet is: "
    print pt
    putStr "The product of it is: "
    print answer
    


