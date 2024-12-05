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

instance Monad (MyState s) where
    (MyState f) >>= g = MyState $ \s0 ->
        let (a, s1) = f s0      -- 入力となる状態 s0 を使用して f を評価して (a, s1) を得る
            (MyState g') = g a  -- a を使用して次に実行する関数を準備
        in g' s1                -- s1 を使用して次に実行する関数を実行
        -- in runState (g a) s1 -- このようにしても書ける．まず runState によって MyState s b から s -> (b, s) を得て，これに対して s1 を与えて実行している．

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
