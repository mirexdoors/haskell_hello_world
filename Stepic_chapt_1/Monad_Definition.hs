import Control.Monad
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

{-Реализуйте фукцию bindLog

bindLog :: Log a -> (a -> Log b) -> Log b
которая работает подобно оператору >>= для контекста Log.-}

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log msga a) f = Log (msga ++ msgb) b where
  (Log msgb b) = f a

{-
Реализованные ранее returnLog и bindLog позволяют объявить тип Log представителем класса Monad:

instance Monad Log where
    return = returnLog
    (>>=) = bindLog
Используя return и >>=, определите функцию execLoggersList

execLoggersList :: a -> [a -> Log a] -> Log a
которая принимает некоторый элемент, список функций с логированием и возвращает результат последовательного применения всех функций в списке к переданному элементу вместе со списком сообщений, которые возвращались данными функциями:

GHCi> execLoggersList 3 [add1Log, mult2Log, \x -> Log ["multiplied by 100"] (x * 100)]
Log ["added one","multiplied by 2","multiplied by 100"] 800-}

instance Monad Log where
    return = returnLog
    (>>=) = bindLog

instance Functor Log where
  fmap = liftM

instance Applicative Log where
  pure = return
  (<*>) = ap

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList = foldl (>>=) . return

test1 = execLoggersList 3 [add1Log, mult2Log, \x -> Log ["multiplied by 100"] (x * 100)]
