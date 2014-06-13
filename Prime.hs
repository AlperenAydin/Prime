module Prime
       (testPrime, testPrime')
       where


fi=fromIntegral

--This is a function that returns a list of 2 and 3 and
--all the integer of form 6k+/-1 less than the sqrt of p
intervalle :: Int -> [Int]
intervalle p = 2:3:(takeWhile (\ k -> (fi k) < 1.0+ sqrt(fi p)) [6*k +l | k<- [1..], l<-[-1,1]])


--This function tells us if this a number is a prime or not.
--A prime number is not divisble by 2, 3 and the integers of form 6k+/-1 less than the sqrt of p.
--Hence the use of intervalle.
isprime :: Int -> Bool
isprime 2 = True
isprime 3 = True
isprime p = all (/=0) $ map (\ n -> p `mod` n) (intervalle p)

-- These two functions check if a set is made of primes or not.
--1st one gives a general answer
testPrime :: [Int] -> Maybe Bool
testPrime [] = Nothing
testPrime l = Just $ all isprime l

--While the 2nd one return a list telling the primality of each element
testPrime' :: [Int] -> Maybe [(Int, Bool)]
testPrime' [] = Nothing
testPrime' xs = Just  [(x, isprime x) | x<- xs]

-- implement for test which uses testprime testprime'
