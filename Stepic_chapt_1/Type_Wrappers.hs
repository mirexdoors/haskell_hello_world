{-Реализуйте представителя класса типов Monoid для типа Xor, в котором mappend выполняет операцию xor.-}

import Data.Semigroup

newtype Xor = Xor { getXor :: Bool }
  deriving (Eq,Show)

instance Semigroup Xor where
   Xor a  <>  Xor b = Xor (a /= b)

instance Monoid Xor where
  mempty = Xor False
  mappend = (Data.Semigroup.<>)

----------------------------------------------------------
{-
Реализуйте представителя класса типов Monoid для Maybe' a так, чтобы mempty не был равен Maybe' Nothing.
 Нельзя накладывать никаких дополнительных ограничений на тип a, кроме указанных в условии.-}

newtype Maybe' a = Maybe' { getMaybe :: Maybe a }
    deriving (Eq, Show)

instance Semigroup a => Semigroup (Maybe' a) where
    Maybe' (Just x) <> Maybe' (Just y) = Maybe' (Just (x <> y))
    _  <> _                            = Maybe' Nothing

instance Monoid a => Monoid (Maybe' a) where
    mempty = Maybe' (Just mempty)
