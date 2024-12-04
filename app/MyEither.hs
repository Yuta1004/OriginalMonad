module MyEither where

-- Functor
--  - fmap :: (a -> b) -> f a -> f b

-- Applicative
--  - pure :: a -> f a
--  - (<*>) :: f (a -> b) -> f a -> f b

-- Monad
--  - (>>=) :: m a -> (a -> m b) -> m b

data MyEither a b = Err a | Ok b
    deriving (Show, Eq)

instance Functor (MyEither a) where
    fmap f (Ok x) = Ok (f x)    -- Ok: (c -> d) -> Either a c -> Either a d
    fmap  _ (Err x) = Err x     -- Ok: (c -> d) -> Either a c -> Either a d
--  fmap f (Err x) = Err (f x)  -- Ng: (c -> d) -> Either c ? -> Either d ?

instance Applicative (MyEither a) where
    pure = Ok
    (Ok f) <*> (Ok x) = Ok (f x)
    (Err f) <*> _ = Err f
    _ <*> (Err x) = Err x

instance Monad (MyEither a) where
    (Ok x) >>= f = f x
    (Err x) >>= _ = Err x

mydiv :: Int -> Int -> MyEither () Int
mydiv _ 0 = Err ()
mydiv a b = Ok (a `div` b)

runMyResult :: MyEither () Int
runMyResult = do
    lhs <- Ok 20
    rhs <- Ok 10
    lhs `mydiv` rhs
