import Data.IntSet as IntSet

hasFactor :: Int -> Int -> Bool
hasFactor n x = n `mod` x == 0

factors :: Int -> IntSet
factors n = fromList $ Prelude.filter (hasFactor n) [1..n]

prime :: Int -> Bool
prime n = factors n == fromList [1, n]

primeFactors :: Int -> IntSet
primeFactors = IntSet.filter prime . factors

euler3 = findMax . primeFactors

main = print (euler3 600851475143)
