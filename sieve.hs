module Sieve
       where

import Prime

--This is the implemtation of Sieve of Eratho.
--http://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
--It works by removing all the multiples of x from xs
sievelist :: [Int] -> [Int]
sievelist [] = []
sievelist (x:xs) = x:sievelist [ k | k <-xs, k `mod` x /=0 ]


sieve ::Int -> [Int]
sieve p = sievelist [2..p]

