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

