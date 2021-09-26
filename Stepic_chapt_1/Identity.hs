import Control.Monad

data SomeType a = SomeType a

instance Monad SomeType where
    return = return
    (>>=) = (>>=)

instance Applicative SomeType where
  pure = return
  (<*>) = ap

instance Functor SomeType where
    fmap f x = x >>= return . f
