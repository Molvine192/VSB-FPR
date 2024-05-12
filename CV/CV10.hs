data Tree a = Leaf a | Internal a (Tree a) (Tree a) deriving(Show)

tree1 :: Tree Int
tree1 = Internal 10 (Internal 5 (Leaf 7) (Leaf 6)) (Leaf 8)

sum' :: Tree Int -> Int
sum' (Leaf v) = v
sum' (Internal v lt rt) = v + sum' lt + sum' rt

toList :: Tree a -> [a]
toList (Leaf v) = [v]
toList (Internal v lt rt) = [v] ++ toList lt ++ toList rt

maxTree :: Ord a => Tree a -> a
maxTree (Leaf v) = v
maxTree (Internal v lt rt) = max v (max (maxTree lt) (maxTree rt))

depthTree :: Tree a -> Int
depthTree (Leaf v) = 1
depthTree (Internal v lt rt) = max (depthTree lt) (depthTree rt) + 1

getGreaterElements :: Ord a => Tree a -> a -> [a]
getGreaterElements tree a = filter (> a) $ toList tree

toString :: Show a => Tree a -> String
toString (Leaf v) = show v
toString (Internal v lt rt) = show v ++ "(" ++ toString lt ++ "," ++ toString rt ++ ")"

-- fromString :: Read a => String -> Tree a
-- fromString (x:xs) = 

leafCount :: Tree a -> Int
leafCount (Leaf v) = 1
leafCount (Internal v lt rt) = leafCount lt + leafCount rt

branchCount :: Tree a -> Int
branchCount (Leaf v) = 0
branchCount (Internal v lt rt) = 1 + branchCount lt + branchCount rt

contains :: Eq a => Tree a -> a -> Bool
contains (Leaf v) a = v == a
contains (Internal v lt rt) a = v == a || contains lt a || contains rt a

greaterThan :: Ord a => Tree a -> a -> Int
greaterThan tree a = length $ filter (> a) $ toList tree