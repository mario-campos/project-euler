import Data.IntSet (toList, fromDistinctAscList, union)

multiples :: Int -> [Int]
multiples x = iterate (+x) x

euler1 n = sum $ toList $ union mults3 mults5
  where mults3 = fromDistinctAscList $ takeWhile (<n) (multiples 3)
        mults5 = fromDistinctAscList $ takeWhile (<n) (multiples 5)

main = print (euler1 1000)
