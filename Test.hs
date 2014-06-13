module Test
       where
import Prime

test1 = testPrime [] == Nothing
test2 = testPrime [512] == Just False
test3 = testPrime [2, 3, 5 , 17] == Just True
test4 = testPrime [93179] == Just True

test = all (== True) [test1, test2, test3, test4 ]
