module Main where

-- Functor
--  - fmap :: (a -> b) -> f a -> f b
--      =

-- Applicative
--  - pure :: a -> f a
--      =
--  - (<*>) :: f (a -> b) -> f a -> f b
--      =

-- Monad
--  - (>>=) :: m a -> (a -> m b) -> m b
--      =

data MyMaybe a = Some a | None
    deriving (Show, Eq)

instance Functor MyMaybe where
    fmap f (Some a) = Some (f a)
    fmap _ None = None

instance Applicative MyMaybe where
    pure = Some
    (Some f) <*> (Some a) = Some (f a)
    _ <*> _ = None

instance Monad MyMaybe where
    (Some a) >>= f = f a
    None >>= _ = None

mymain :: MyMaybe Int
mymain = do
    a <- Some 10
    b <- Some 20
    return $ a + b

main :: IO ()
main = print mymain
