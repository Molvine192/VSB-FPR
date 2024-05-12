import Data.List

convert :: [(String, Int, Float)] -> [(String,Float)]
convert [] = []
convert ((name,count,price):xs) = [(name, price * (fromIntegral count))] ++ convert xs

replaceByRepeat :: String -> Char -> Int -> String
replaceByRepeat [] _ _ = []
replaceByRepeat (x:xs) c n = if x == c then (replicate n c) ++ replaceByRepeat xs c n else [x] ++ replaceByRepeat xs c n 

change :: [a] -> [(Int,Int)] -> [a]
change x [] = x
change x ((a,b):rest) = change reversed rest where
    reversed = (take a x) ++ (reverse (drop a (take (a+b) x))) ++ (drop (a+b) x)