length' :: [a] -> Int
length' []  = 0
length' (_:xs) = 1 + length' xs

sumIt :: [Int] -> Int
sumIt []  = 0
sumIt (x:xs) = x + sumIt xs

getHead :: [a] -> a
getHead (x:_) = x

getLast :: [a] -> a
getLast [x] = x
getLast (x:xs) = getLast xs

isElement :: Eq a => a -> [a] -> Bool
isElement _ [] = False
isElement a (x:xs) | a == x = True 
                   | otherwise = isElement a xs

getTail :: [a] -> [a]                   
getTail (_:xs) = xs

getInit :: [a] -> [a]
getInit [_] = []
getInit (x:xs) = x : getInit xs

combine :: [a] -> [a] -> [a]
combine [] y = y
combine (x:xs) y = x : combine xs y

max' :: [Int] -> Int
max' [x] = x
max' (x:y:z) | x > y = max' (x:z)
             | otherwise = max' (y:z)

reverse' :: [a] -> [a]             
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

scalar :: [Int] -> [Int] -> Int
scalar [] [] = 0
scalar (x:xs) (y:ys) = x*y + scalar xs ys