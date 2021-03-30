{-
Тип данных Color определен следующим образом

data Color = Red | Green | Blue
Определите экземпляр класса Show для типа Color, сопоставляющий каждому из трех цветов его текстовое представление.

GHCi> show Red
"Red"-}


data Color = Red | Green | Blue

instance Show Color where
    show Red = "Red"
    show Green = "Green"
    show Blue = "Blue"


data LogLevel = Error | Warning | Info

cmp :: LogLevel -> LogLevel -> Ordering
cmp Error Warning = GT
cmp Error Info = GT


cmp Warning Error = LT
cmp Warning Info  = GT

cmp Info Warning = LT
cmp Info Error = LT

cmp Info Info = EQ
cmp Error Error = EQ
cmp Warning Warning = EQ

-----------------------------------------------------
{-
Пусть объявлен следующий тип данных:

data Result = Fail | Success


И допустим определен некоторый тип данных SomeData и некоторая функция
doSomeWork :: SomeData -> (Result,Int)
возвращающая результат своей работы и либо код ошибки в случае неудачи, либо 0 в случае успеха.
Определите функцию processData, которая вызывает doSomeWork и возвращает строку "Success" в случае ее успешного завершения,
 либо строку "Fail: N" в случае неудачи, где N — код ошибки.-}

data Result = Fail | Success

data SomeData

doSomeWork :: SomeData -> (Result,Int)
doSomeWork = undefined

processData :: SomeData -> String
processData d = case doSomeWork d of
                        (Success, _) -> "Success"
                        (Fail, errNum) -> "Fail: " ++ show errNum


------------------------------------------------------------

data Point = Point Double Double

origin :: Point
origin = Point 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt (x ^ 2 + y ^ 2)

distance :: Point -> Point -> Double
distance (Point a b) (Point x y) = sqrt ((a - x)**2 + (b - y)**2)