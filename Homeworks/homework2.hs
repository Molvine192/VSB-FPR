import Data.List
import Data.Ord

type Schedule = [Point]
data Point = Point { getX :: Int, getY :: Int } deriving (Eq, Show)

task1 = [Point 1 1,
         Point 10 10,
         Point 8 1,
         Point 1 8,
         Point 8 2,
         Point 7 2]

task2 = [Point 10 10]

task3 = [Point 0 10,
         Point 10 0,
         Point 10 10]

-- Вычисляет расстояние между двумя точками
dist :: Point -> Point -> Int
dist p1 p2 = abs (getX p1 - getX p2) + abs (getY p1 - getY p2)

-- Вычисляет общее расстояние для заданного порядка точек
calculateDist :: [Point] -> Int
calculateDist [] = 0
calculateDist [p] = 2 * (abs (getX p) + abs (getY p))
calculateDist (p1:p2:ps) = dist p1 p2 + calculateDist (p2:ps)

-- Возвращает все возможные комбинации списка точек
combinations :: Eq a => [a] -> [[a]]
combinations [] = [[]]
combinations xs = [x:ys | x <- xs, ys <- combinations (filter (/= x) xs)]

scheduleDrilling :: [Point] -> ([Point], Int)
scheduleDrilling points = minimumBy (comparing snd) schedules
  where
    schedules = [(schedule, calculateDist schedule) | schedule <- combinations points]