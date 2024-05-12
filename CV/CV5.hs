import Data.Char

oddList :: Int -> Int -> [Int]
oddList a b = [x | x <- [a..b], odd x]

removeAllUpper :: String -> String
removeAllUpper xs = [x | x <- xs, not (isUpper x)]

union :: Eq a => [a] -> [a] -> [a]
union xs ys = xs ++ [y | y <- ys, not (elem y xs)]

intersection :: Eq a => [a] -> [a] -> [a]
intersection xs ys = [s | s <- xs, elem s xs && elem s ys]

unique :: [Char] -> [Char]
unique s = tmp [] s where
    tmp xs [] = xs
    tmp xs (y:ys) | elem y xs = tmp xs ys
                  | otherwise = tmp (xs ++ [y]) ys

frequency :: Char -> String -> Int
frequency ch [] = 0
frequency ch (x:xs) = if ch == x then 1 + frequency ch xs else frequency ch xs

countThem :: String -> [(Char, Int)]
countThem s = tmp [] (unique s) where
    tmp xs [] = xs
    tmp xs (y:ys) = xs ++ [(y,frequency y s)] ++ tmp xs ys

countThem' :: String -> [(Char, Int)]
countThem' s = [(x, frequency x s) | x <- unique s]

isPrime :: Int -> Bool
isPrime 1 = False
isPrime y = isPrimeTest y (y-1) where
  isPrimeTest _ 1 = True 
  isPrimeTest n x | n `mod` x ==0 = False
                  | otherwise = isPrimeTest n (x-1)

goldbach :: Int -> [(Int, Int)]
goldbach a = [(x, a-x) | x <- [1..(div a 2)], isPrime x, isPrime (a-x), x + a-x == a]

goldbachList :: Int -> Int-> Int -> [(Int, Int)]
goldbachList a b limit = filter (\(x,_) -> x > limit) [head (goldbach x) | x <- [a..b], even x]