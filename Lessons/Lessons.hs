fib :: (Num a, Eq a) => a -> a
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fastFib n = take n fiblist
  where fiblist = 0:1:(zipWith (+) fiblist (tail fiblist))


factorial n = product [1..n]

main :: IO()
main = putStrLn (show (fastFib 100000)) 