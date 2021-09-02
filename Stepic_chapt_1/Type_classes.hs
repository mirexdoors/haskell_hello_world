--Реализуйте класс типов Printable, предоставляющий один метод toString — функцию одной переменной, которая преобразует значение типа, являющегося представителем Printable, в строковое представление.
--
--Сделайте типы данных Bool и () представителями этого класса типов, обеспечив следующее поведение:
--
--GHCi> toString True
--"true"
--GHCi> toString False
--"false"
--GHCi> toString ()
--"unit type"

class Printable a where
  toString :: a -> [Char]

instance Printable Bool where
 toString True = "true"
 toString False = "false"

instance Printable () where
  toString ()  = "unit type"

--Сделайте тип пары представителем класса типов Printable, реализованного вами в предыдущей задаче, обеспечив следующее поведение:
--
--GHCi> toString (False,())
--"(false,unit type)"
--GHCi> toString (True,False)
--"(true,false)"

instance (Printable p1, Printable p2) => Printable (p1, p2) where
  toString (p1, p2) = "(" ++  toString p1 ++ "," ++ toString p2 ++ ")"
