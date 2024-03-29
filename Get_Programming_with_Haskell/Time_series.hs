import Data.List
import qualified Data.Map as Map
import Data.Semigroup
import Data.Maybe

-- Моковые данные
file1 :: [(Int, Double)]
file1 = [(1, 200.1), (2, 199.5), (3, 199.4), (4, 198.9), (5, 199.0), (6, 200.2), (9, 200.3), (10, 201.2), (12, 202.9)]

file2 :: [(Int, Double)]
file2 = [(11, 201.6), (12, 201.5), (13, 201.5), (14, 203.5), (15, 204.9), (16, 201.7), (18, 210.5), (20, 208.8)]

file3 :: [(Int, Double)]
file3 = [(10, 201.2), (11, 201.6), (12, 201.5), (13, 201.5), (14, 203.5), (17, 210.5), (24, 215.1), (25, 218.7)]

file4 :: [(Int, Double)]
file4 = [(26, 219.8), (27, 220.5), (28, 223.8), (29, 222.8), (30, 223.8), (31, 221.7), (32, 222.3), (33, 220.8), (34, 219.4), (35, 220.1), (36,220.6)]


-- Построение базового типа временного ряда. 1-й параметро типа - отметка времени, 2-й значение
data TS a = TS [Int] [Maybe a]

{-
При вызове функции createTS временная линия насыщается значениями и становится непрерывной. Затем из имеющихся значений создаётся Map.
После этого проходим по полному списку дат и ищем в Map соответсвующие значения.
Это автоматически создаёт список значений типа Maybe, где существующие значения будут Just, а отсутствующие - Nothing
-}
createTS :: [Int] -> [a] -> TS a
createTS times values = TS completeTimes extendedValues where
  completeTimes = [minimum times .. maximum times]
  timeValueMap = Map.fromList(zip times values)
  extendedValues = map (\v -> Map.lookup v timeValueMap) completeTimes

{-
Теперь нам необходимо раскрыть пары из данных в значения TS
-}
fileToTS :: [(Int, Double)] -> TS Double
fileToTS tvPairs = createTS times values where
  (times, values) = unzip tvPairs

{-
Определяем экземпляр Show для типа TS
-}
showTVPair :: Show a => Int -> Maybe a -> String
showTVPair time (Just value)  = mconcat [show time, "|", show value, "\n"]
showTVPair time Nothing = mconcat [show time, "|NA\n"]

instance Show a => Show (TS a) where
  show (TS times values) = mconcat rows where
    rows = zipWith showTVPair times values

{-
Приводим все данные к типу TS
-}
ts1 :: TS Double
ts1 = fileToTS file1

ts2 :: TS Double
ts2 = fileToTS file2

ts3 :: TS Double
ts3 = fileToTS file3

ts4 :: TS Double
ts4 = fileToTS file4

{-
Теперь нам нужно научиться "сшивать" временные ряды. Нужна функция, которая принимает два значения TS и возвращает одно
Мы не можем просто конкатенировать списки, т.к. у нас есть пересекающиеся значения и порядок значений может быть разный.
Воспользуемся для этого Map
-}
-- Вспомогательная функция для вставки данных в Map
insertMaybePair :: Ord k=> Map.Map k v -> (k, Maybe v) -> Map.Map k v
insertMaybePair myMap (_, Nothing) = myMap
insertMaybePair myMap (key, Just value) = Map.insert key value myMap


{-
Теперь мы можем комбинировать все ряды в один
-}
combineTS :: TS a -> TS a -> TS a
combineTS (TS [] []) ts2 = ts2
combineTS ts1 (TS [] []) = ts1
combineTS (TS t1 v1) (TS t2 v2) = TS completeTimes combinedValues where
  bothTimes = mconcat [t1, t2]
  completeTimes = [minimum bothTimes .. maximum bothTimes]
  tvMap = foldl insertMaybePair Map.empty (zip t1 v1)
  updatedMap = foldl insertMaybePair tvMap (zip t2 v2)
  combinedValues = map (\v -> Map.lookup v updatedMap) completeTimes

instance Semigroup (TS a) where
  (<>) = combineTS

{-
Делаем TS экземпляром Monoid, чтобы иметь возможность комбинировать список [TS a] -> TS a
-}
instance Monoid (TS a) where
  mempty = TS [] []
  mappend = (<>)

tsAll :: TS Double
tsAll = mconcat [ts1, ts2, ts3, ts4]

{-
Реализуем функцию, вычисляющую среднее значение числового списка
-}
mean :: (Real a) => [a] -> Double
mean xs = total/count where
  total = (realToFrac . sum) xs
  count = (realToFrac . length) xs

meanTS :: (Real a) => TS a -> Maybe Double
meanTS (TS _ []) = Nothing
meanTS (TS times values) =
  if all (== Nothing) values
  then Nothing
  else Just avg
    where justVals = filter isJust values
          cleanVals = map fromJust justVals
          avg = mean cleanVals

{-
Напишем функцию для сравнения двух значений временного ряда
-}
type CompareFunc a = a -> a -> a
type TSCompareFunc a = (Int, Maybe a) -> (Int, Maybe a) -> (Int, Maybe a)

makeTSCompare :: Eq a => CompareFunc a -> TSCompareFunc a
makeTSCompare func = newFunc
  where
    newFunc (i1, Nothing) (i2, Nothing) = (i1, Nothing)
    newFunc (_, Nothing) (i, val) = (i, val)
    newFunc (i, val) (_, Nothing) = (i, val)
    newFunc (i1, Just val1) (i2, Just val2) =
      if func val1 val2 == val1
      then (i1, Just val1)
      else (i2, Just val2)
{-
Теперь мы можем построить общую функцию, которая позволит сравнивать все значения в TS
-}
compareTS :: Eq a => (a->a->a) -> TS a -> Maybe (Int, Maybe a)
compareTS func (TS [] []) = Nothing
compareTS func (TS times values) =
  if all (== Nothing) values
  then Nothing
  else Just best
    where
      pairs = zip times values
      best = foldl (makeTSCompare func) (0, Nothing) pairs

minTS :: Ord a => TS a -> Maybe (Int, Maybe a)
minTS = compareTS min

maxTs :: Ord a => TS a -> Maybe (Int, Maybe a)
maxTs = compareTS max

{-
Вычислим разности временных рядов (дельты значений)
-}
diffPair :: Num a => Maybe a -> Maybe a -> Maybe a
diffPair _ Nothing = Nothing
diffPair Nothing _ = Nothing
diffPair (Just x) (Just y) = Just (x - y)

{-
Теперь определим функцию требуемого преобразования diffTS
-}
diffTS :: Num a => TS a -> TS a
diffTS (TS [] []) = TS [] []
diffTS (TS times values) = TS times (Nothing : diffValues)
  where shiftValues = tail values
        diffValues = zipWith diffPair shiftValues values

{-
Теперь мы можем посчитать, например, среднее значений измений в месячных обьемах продаж на протяжение всего промежутка времени
-}
avgDiffTS = meanTS (diffTS tsAll)

{-
Теперь построим функциии для получения скользящего среднего.
Для начала упрости реализацию последущих функций, опередлим функцию скользящего среднего, которая работает
со список значений временного ряда вида [Maybe a]
-}

meanMaybe :: (Real a) => [Maybe a] -> Maybe Double
meanMaybe vals = if any (== Nothing) vals then Nothing else (Just avg)
  where avg = mean (map fromJust vals)

movingAvg :: (Real a) => [Maybe a] -> Int -> [Maybe Double]
movingAvg [] n = []
movingAvg vals n =
  if length nextVals == n
  then meanMaybe nextVals : movingAvg restVals n
  else []
    where nextVals = take n vals
          restVals = tail vals

{-
Вычисление скользящего среднего с центрированием
-}
movingAverageTS :: (Real a) => TS a -> Int -> TS Double
movingAverageTS (TS [] []) n = TS [] []
movingAverageTS (TS times values) n = TS times smoothedValues
  where
    ma = movingAvg values n
    nothings = replicate (n `div` 2) Nothing
    smoothedValues = mconcat [nothings, ma, nothings]
