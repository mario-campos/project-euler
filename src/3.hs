import Data.IntSet as IntSet

divides :: Int -> Int -> Bool
divides n x = n `mod` x == 0

factors :: Int -> IntSet
factors n = fromList $ Prelude.filter (divides n) [1..n]

prime :: Int -> Bool
prime n = factors n == fromList [1, n]

primeFactors :: Int -> IntSet
primeFactors = IntSet.filter prime . factors

euler3 = findMax . primeFactors

main = print (euler3 600851475143)
