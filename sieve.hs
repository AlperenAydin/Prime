module Sieve
       where

import Prime
import Data.List

--This is the implemtation of Sieve of Eratho.
--http://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
--It works by removing all the multiples of x from xs
sievelist :: [Int] -> [Int]
sievelist [] = []
sievelist (x:xs) = x:sievelist [ k | k <-xs, k `mod` x /=0 ]

sieve ::Int -> [Int]
sieve p = sievelist [2..p]

--Function used for testing sieve
--Functions used for the testing are in Prime.hs
testSieve :: Int -> Maybe Bool
testSieve = testPrime.sieve


testSieve' :: Int -> Maybe [(Int, Bool)]
testSieve' = testPrime'.sieve

--A more efficient version of sieve

siev :: [Int] -> [Int]
siev [] = []
siev l = let (he, ta) = span (/=head l ^ 2) l
           in he ++ siev [k | k<-ta, all (/=0) $ map (\ p -> k `mod` p) he ]

erathos = siev [2.. ]
