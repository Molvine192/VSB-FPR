import Data.List
import Data.Maybe

type Result = [String]

pp :: Result -> IO ()
pp x = putStr (unlines x)

ticktack :: (Int, Int) -> [(Int,Int)] -> Result
ticktack (x,y) moves = replicate (x + 2) '-' : reverse ([row r | r <- [1..y]]) ++ [replicate (x + 2) '-'] where
    row r =  "|" ++ [cell (c, r) | c <- [1..x]] ++ "|"
    cell (c, r)     | (c, r) `elem` moves = if even (fromJust (elemIndex (c,r) moves) + 1) then 'o' else 'x'
                    | otherwise = ' '