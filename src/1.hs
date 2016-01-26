import Data.IntSet (toList, fromDistinctAscList, union)

multiples :: Int -> [Int]
multiples x = iterate (+x) x

euler1 n = sum $ toList $ union mults3 mults5
  where mults3 = fromDistinctAscList $ filter (<n) (multiples 3)
        mults5 = fromDistinctAscList $ filter (<n) (multiples 5)

main = euler1 1000
