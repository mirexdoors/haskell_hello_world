{-
Определите тип записи, который хранит элементы лога. Имя конструктора должно совпадать с именем типа, и запись должна содержать три поля:
timestamp — время, когда произошло событие (типа UTCTime);
logLevel — уровень события (типа LogLevel);
message — сообщение об ошибке (типа String).
Определите функцию logLevelToString, возвращающую текстуальное представление типа LogLevel, и функцию logEntryToString, возвращающую текстуальное представление записи в виде:

<время>: <уровень>: <сообщение>


Для преобразование типа UTCTime в строку используйте функцию timeToString.-}
import Data.Time.Clock
import Data.Time.Format
import System.Locale
import Data.Function

timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info deriving Show

data LogEntry = LogEntry { timestamp :: UTCTime, logLevel :: LogLevel, message :: String } deriving Show

logLevelToString :: LogLevel -> String
logLevelToString a = show a :: String

logEntryToString :: LogEntry -> String
logEntryToString entry =(timeToString $(entry & timestamp)) ++ ": " ++ (logLevelToString $ (entry & logLevel)) ++ ": " ++ entry & message

(&) x f = f x

---------------------------------------------------------
{-
Определите функцию updateLastName person1 person2, которая меняет фамилию person2 на фамилию person1-}

data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving Show

updateLastName :: Person -> Person -> Person
updateLastName person1 person2 = person2 {lastName = lastName person1 }

person1 = Person {firstName = "Ivan", lastName = "Ivanov", age = 11 }
person2 = Person {firstName = "Mikhail", lastName = "Mikhailov", age = 2 }
person3 = Person {firstName = "Mi", lastName = "Mikhailov", age = 2 }


------------------------------------------------------------------------------------------------------

{-
Определить функцию abbrFirstName, которая сокращает имя до первой буквы с точкой, то есть, если имя было "Ivan", то после применения этой функции оно превратится в "I.".
Однако, если имя было короче двух символов, то оно не меняется.-}

abbrFirstName :: Person -> Person
abbrFirstName p
	 | (length $ firstName p) > 2 = p { firstName = cut $ firstName p }
	 | otherwise = p

cut :: String -> String
cut str= take 2 str ++ "."
