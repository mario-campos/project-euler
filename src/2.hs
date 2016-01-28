fib :: [Int]
fib = 0 : 1 : [ a + b | (a, b) <- zip fib (tail fib) ]

euler2 n = sum . filter even $ takeWhile (<n) fib

main = print (euler2 4000000)
