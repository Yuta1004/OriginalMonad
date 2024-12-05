module MyState where

-- Functor
--  - fmap :: (a -> b) -> f a -> f b

-- Applicative
--  - pure :: a -> f a
--  - (<*>) :: f (a -> b) -> f a -> f b

-- Monad
--  - (>>=) :: m a -> (a -> m b) -> m b

newtype MyState s a = MyState { runState :: s -> (a, s) }

instance Functor (MyState s) where
    fmap f (MyState g) = MyState $ \s0 ->
        let (a, s1) = g s0
        in (f a, s1)

instance Applicative (MyState s) where
    pure a = MyState (a, )
    (MyState f) <*> (MyState g) = MyState $ \s0 ->
        let (h, s1) = f s0
            (a', s2) = g s1
        in (h a', s2)

instance Monad (MyState a) where
    (MyState f) >>= g = MyState $ \s0 ->
        let (a, s1) = f s0
        in runState (g a) s1

myGet :: MyState s s
myGet = MyState $ \s0 -> (s0, s0)

myPut :: s -> MyState s ()
myPut s = MyState $ const ((), s)

myModify :: (s -> s) -> MyState s ()
myModify f = do
    a <- myGet
    myPut $ f a

evalMyState :: MyState s a -> s -> s
evalMyState m s0 =
    let (_, s) = runState m s0
    in s

execMyState :: MyState s a -> s -> a
execMyState m s0 =
    let (a, _) = runState m s0
    in a

runMyState :: MyState Int String
runMyState = do
    a <- myGet
    myPut $ a + 20
    myModify (*2)
    a' <- myGet
    case a' of
        40 -> return "0"
        _ -> return "1"

runMyState' :: MyState Int String
runMyState' =
    myGet >>= (\a ->
        myPut (a + 20) >>
            myModify (*2) >>
                myGet >>= \a' -> (
                    case a' of
                        40 -> return "0"
                        _ -> return "1"
                )
    )

runMyState'' :: MyState Int String
runMyState'' =
    MyState (\s0 -> (s0, s0)) >>= \a -> (
        MyState (const ((), a + 20)) >>= \_ -> (
            MyState (\s0 -> (s0, s0)) >>= \t -> (
                MyState (const ((), (*2) t)) >>= \_ -> (
                    MyState (\s0 -> (s0, s0)) >>= \a' -> (
                        case a' of
                            40 -> return "0"
                            _ -> return "1"
                    )
                )
            )
        )
    )
