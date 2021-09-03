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

tree = Branch(Branch(Leaf Nothing) Nothing (Branch(Leaf (Just "o o o")) (Just "3 3 3") (Leaf (Just "5 5 5")))) (Just "2 2 2") (Leaf (Just "q q q"))
tree' = Branch(Leaf Nothing) (Nothing) (Leaf Nothing)

instance Functor Tree where
    fmap func Tree = undefined

