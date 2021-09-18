{-
Введём следующий тип:

data Log a = Log [String] a
Реализуйте вычисление с логированием, используя Log. Для начала определите функцию toLogger

toLogger :: (a -> b) -> String -> (a -> Log b)
которая превращает обычную функцию, в функцию с логированием:

GHCi> let add1Log = toLogger (+1) "added one"
GHCi> add1Log 3
Log ["added one"] 4

GHCi> let mult2Log = toLogger (* 2) "multiplied by 2"
GHCi> mult2Log 3
Log ["multiplied by 2"] 6
Далее, определите функцию execLoggers

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c

Которая принимает некоторый элемент и две функции с логированием. execLoggers возвращает результат последовательного применения функций к элементу и список сообщений, которые были выданы при применении каждой из функций:
GHCi> execLoggers 3 add1Log mult2Log
Log ["added one","multiplied by 2"] 8-}

data Log a = Log [String] a deriving Show

mult2Log = toLogger (* 2) "multiplied by 2"
add1Log = toLogger (+1) "added one"

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger func str = Log [str] . func

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers a logFunc1 logFunc2 = Log (log1 ++ log2) c where
  (Log log1 b) = logFunc1 a
  (Log log2 c) = logFunc2 b

----------------------------------------------
{-
Функции с логированием из предыдущего задания возвращают в качестве результата значение с некоторой дополнительной информацией
в виде списка сообщений. Этот список является контекстом. Реализуйте функцию returnLog

returnLog :: a -> Log a

которая является аналогом функции return для контекста Log. Данная функция должна возвращать переданное ей значение с пустым контекстом.-}

returnLog :: a -> Log a
returnLog = Log []


bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log msga a) f = Log (msga ++ msgb) b where
  (Log msgb b) = f a
