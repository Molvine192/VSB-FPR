import Data.Char

zipThem :: [a] -> [b] -> [(a,b)]
zipThem [] [] = []
zipThem _ [] = []
zipThem [] _ = []
zipThem [x] [y] = [(x,y)]
zipThem (x:xs) (y:ys) = [(x,y)] ++ zipThem xs ys

simple :: a -> [b] -> [(a,b)]
simple _ [] = []
simple x (y:ys) = (x,y) : simple x ys

dotProduct :: [a] -> [b] -> [(a,b)]
dotProduct [] _ = []
dotProduct (x:xs) ys = simple x ys ++ dotProduct xs ys

dotProduct2 :: [a] -> [b] -> [(a,b)]
dotProduct2 xs ys = [(x,y) | x<-xs, y<-ys]

allToUpper :: String -> String
allToUpper [] = []
allToUpper xs = map toUpper xs

allToUpper2 :: String -> String
allToUpper2 [] = []
allToUpper2 (x:xs) = toUpper x : allToUpper2 xs

allToUpper3 :: [Char] -> [Char]
allToUpper3 xs = [toUpper x | x<-xs]