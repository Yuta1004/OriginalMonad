module MyContainer where

-- Functor
--  - fmap :: (a -> b) -> f a -> f b

-- Applicative
--  - pure :: a -> f a
--  - (<*>) :: f (a -> b) -> f a -> f b

-- Monad
--  - (>>=) :: m a -> (a -> m b) -> m b

newtype MyContainer a = Value a
    deriving (Show, Eq)

instance Functor MyContainer where
    fmap f (Value x) = Value (f x)

instance Applicative MyContainer where
    pure = Value
    (Value f) <*> (Value x) = Value (f x)

instance Monad MyContainer where
    (Value a) >>= f = f a

c1 :: String -> MyContainer [Char]
c1 _ = pure ['h', 'e', 'l', 'l', 'o']

c2 :: [Char] -> MyContainer [Int]
c2 _ = pure [0, 1, 2, 3, 4]

c3 :: [Int] -> MyContainer Int
c3 _ = pure 0

c4 :: Int -> MyContainer ()
c4 _ = pure ()

runMyContainer :: MyContainer ()
runMyContainer = do
    a <- c1 "hello"
    a' <- c2 a
    a'' <- c3 a'
    c4 a''
