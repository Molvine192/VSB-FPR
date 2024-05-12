factorial  :: Int -> Int
factorial  0 = 1
factorial  n = n * factorial  (n-1)

fib :: Int->Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

leapYear :: Int -> Bool
leapYear x = x `mod` 4 == 0 && x `mod` 100 /= 0 || x `mod` 400 == 0

max2 :: Int -> Int -> Int
max2 x y | x >= y = x
         |otherwise = y

max3 :: Int -> Int -> Int -> Int
max3 x y z = (x `max2` y) `max2` z

combinations :: Int -> Int -> Int
combinations n k = factorial n `div` (factorial k * factorial (n-k))

numberOfRoots :: Int -> Int -> Int -> Int
numberOfRoots a b c = let d = b*b - 4 * a *c
                      in if d<0 then 0 else if d == 0 then 1 else 2

gcd' :: Int -> Int -> Int
gcd' a b | a > b = gcd' (a-b) b
         | a < b = gcd' a (b-a)
         | a==b = a

isPrime :: Int -> Bool
isPrime 1 = False
isPrime y = isPrimeTest y (y-1) where
  isPrimeTest _ 1 = True 
  isPrimeTest n x | n `mod` x ==0 = False
                  | otherwise = isPrimeTest n (x-1)