{-
Реализуйте функции distance, считающую расстояние между двумя точками с вещественными координатами,
 и manhDistance, считающую манхэттенское расстояние между двумя точками с целочисленными координатами.-}

data Coord a = Coord {x :: a, y :: a} deriving Show

distance :: Coord Double -> Coord Double -> Double
distance c1 c2 = sqrt $ (x c2 - x c1)^2 + (y c2 - y c1)^2

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance c1 c2 = fromIntegral (getLength (x c1) (x c2) + getLength (y c1)  (y c2))
						where getLength a b = if (b - a) >= 0 then b-a else a-b

coord1 = Coord {x = 0.0, y = 3.0}
coord2 = Coord {x = 0.0, y = 4.0}

manhCoord1 = Coord {x = 1, y = 3}
manhCoord2 = Coord {x = 1, y = 4}
