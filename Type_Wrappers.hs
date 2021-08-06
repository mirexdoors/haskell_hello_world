{-Реализуйте представителя класса типов Monoid для типа Xor, в котором mappend выполняет операцию xor.-}

import Data.Semigroup

newtype Xor = Xor { getXor :: Bool }
  deriving (Eq,Show)

instance Semigroup Xor where
   Xor a  <>  Xor b = Xor (a /= b)

instance Monoid Xor where
  mempty = Xor False
  mappend = (Data.Semigroup.<>)
