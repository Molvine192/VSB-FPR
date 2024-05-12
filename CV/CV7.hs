type Pic = [String]

pp :: Pic -> IO ()
pp x = putStr (concat (map (++"\n") x))

pic :: Pic
pic = [ "....#....",
        "...###...",
        "..#.#.#..",
        ".#..#..#.",
        "....#....",
        "....#....",
        "....#####"]

flipV :: Pic -> Pic
flipV x = map reverse x

flipH :: Pic -> Pic
flipH x = reverse x

above :: Pic -> Pic -> Pic
above x y = x ++ y

sideBySide :: Pic -> Pic -> Pic
sideBySide (x:xs) (y:ys) = (x ++ y) : sideBySide xs ys
sideBySide _ _ = []

sideBySideZip :: Pic -> Pic -> Pic
sideBySideZip p1 p2 = map (\(x,y) -> x ++ y) (zip p1 p2)

sideBySideZipWith :: Pic -> Pic -> Pic
sideBySideZipWith = zipWith (++)

rowToCol :: String -> Pic
rowToCol xs = map (\x -> [x]) xs

rotateR :: Pic -> Pic
rotateR [x] = rowToCol x
rotateR (x:xs) = sideBySide (rotateR xs) (rowToCol x)

rotateRFold :: Pic -> Pic
rotateRFold p = foldl1 sideBySide (reverse (map rowToCol p))

rotateL :: Pic -> Pic
rotateL [x] = reverse (rowToCol x)
rotateL (x:xs) = sideBySide (reverse (rowToCol x)) (rotateL xs)

zoom :: Int -> Pic -> Pic
zoom n xs = [concat(map (replicate n) x)|x<-concat (map (replicate n) xs)]