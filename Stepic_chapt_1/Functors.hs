import Data.Maybe
import Data.Functor
{-
Определите представителя класса Functor для следующего типа данных, представляющего точку в трёхмерном пространстве:

data Point3D a = Point3D a a a deriving Show

GHCi> fmap (+ 1) (Point3D 5 6 7)
Point3D 6 7 8-}

data Point3D a = Point3D a a a deriving Show

instance Functor Point3D where
    fmap f  (Point3D a b c) = (Point3D (f $ a) (f $ b) (f $ c))


--------------------------------
{-Определите представителя класса Functor для типа данных GeomPrimitive, который определён следующим образом:

data GeomPrimitive a = Point (Point3D a) | LineSegment (Point3D a) (Point3D a)
При определении, воспользуйтесь тем, что Point3D уже является представителем класса Functor.

GHCi> fmap (+ 1) $ Point (Point3D 0 0 0)
Point (Point3D 1 1 1)

GHCi> fmap (+ 1) $ LineSegment (Point3D 0 0 0) (Point3D 1 1 1)
LineSegment (Point3D 1 1 1) (Point3D 2 2 2)-}


data GeomPrimitive a = Point (Point3D a) | LineSegment (Point3D a) (Point3D a) deriving Show

instance Functor GeomPrimitive where
    fmap f (Point a) = Point (fmap f a)
    fmap f (LineSegment a b) = LineSegment (fmap f a) (fmap f b)

----------------------------------------------

{-data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) deriving Show

GHCi> words <$> Leaf Nothing
Leaf Nothing

GHCi> words <$> Leaf (Just "a b")
Leaf (Just ["a","b"])-}

data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) deriving Show

tree = Branch (Branch (Leaf Nothing) Nothing (Branch(Leaf (Just "o o o")) (Just "3 3 3") (Leaf (Just "5 5 5")))) (Just "2 2 2") (Leaf (Just "q q q"))
tree' = Branch(Leaf Nothing) (Nothing) (Leaf Nothing)

singleLeaf = Leaf Nothing

instance Functor Tree where
    fmap func (Leaf a) = Leaf (func <$> a)
    fmap func (Branch l x r) = Branch (fmap func l) (func <$> x) (fmap func r)

--------------------------------------------------
{-Определите представителя класса Functor для типов данных Entry и Map. Тип Map представляет словарь, ключами которого являются пары:

data Entry k1 k2 v = Entry (k1, k2) v  deriving Show
data Map k1 k2 v = Map [Entry k1 k2 v]  deriving Show

В результате должно обеспечиваться следующее поведение: fmap применяет функцию к значениям в словаре, не изменяя при этом ключи.

GHCi> fmap (map toUpper) $ Map []
Map []

GHCi> fmap (map toUpper) $ Map [Entry (0, 0) "origin", Entry (800, 0) "right corner"]
Map [Entry (0,0) "ORIGIN",Entry (800,0) "RIGHT CORNER"]-}

data Entry k1 k2 v = Entry (k1, k2) v  deriving Show
data Map k1 k2 v = Map [Entry k1 k2 v]  deriving Show

instance Functor (Entry k1 k2) where
    fmap func (Entry (k1, k2) v) = Entry (k1, k2) (func v)

instance Functor (Map k1 k2) where
    fmap func (Map entries) = Map(map (fmap func) entries)
