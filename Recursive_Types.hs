{-
Тип List, определенный ниже, эквивалентен определению списков из стандартной библиотеки в том смысле,
 что существуют взаимно обратные функции, преобразующие List a в [a] и обратно. Реализуйте эти функции.-}

data List a = Nil | Cons a (List a) deriving (Show)

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons x xs) = x : (fromList xs)

toList :: [a] -> List a
toList [] = Nil
toList (x:xs) = Cons x (toList xs)

-------------------------------------------------------------

{-
Рассмотрим еще один пример рекурсивного типа данных:

data Nat = Zero | Suc Nat
Элементы этого типа имеют следующий вид: Zero, Suc Zero, Suc (Suc Zero), Suc (Suc (Suc Zero)), и так далее.
Таким образом мы можем считать, что элементы этого типа - это натуральные числа в унарной системе счисления.

Мы можем написать функцию, которая преобразует Nat в Integer следующим образом:

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1
Реализуйте функции сложения и умножения этих чисел, а также функцию, вычисляющую факториал.-}

data Nat = Zero | Suc Nat

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

add :: Nat -> Nat -> Nat
add Zero x = x
add (Suc x) y = add x (Suc y)

mul :: Nat -> Nat -> Nat
mul Zero _ = Zero
mul (Suc x) y = add (mul x y) y

fac :: Nat -> Nat
fac Zero = Suc Zero
fac n1@(Suc n) = mul n1 (fac n)

--------------------------------------------------------------------

{-
Тип бинарных деревьев можно описать следующим образом:

data Tree a = Leaf a | Node (Tree a) (Tree a)

Реализуйте функцию height, возвращающую высоту дерева, и функцию size,
 возвращающую количество узлов в дереве (и внутренних, и листьев). Считается, что дерево, состоящее из одного листа, имеет высоту 0.
 -}

data Tree a = Leaf a | Node (Tree a) (Tree a)

height :: Tree a -> Int
height (Leaf _) = 0
height (Node a b) = 1 + max (height a) (height b)

size :: Tree a -> Int
size (Leaf _) = 1
size (Node a b) = 1 + size a + size b

exampleTree =  Node (Leaf 1 ) (Leaf 1)
