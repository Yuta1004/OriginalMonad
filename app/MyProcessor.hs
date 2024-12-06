module MyProcessor where

-- Functor
--  - fmap :: (a -> b) -> f a -> f b

-- Applicative
--  - pure :: a -> f a
--  - (<*>) :: f (a -> b) -> f a -> f b

-- Monad
--  - (>>=) :: m a -> (a -> m b) -> m b

data ParseState = ParseState
    { steps :: Int,
      remain :: String
    }
    deriving (Show, Eq)

------------------------------------

newtype MyProcessor a = MyProcessor
    { step :: ParseState -> (a, ParseState)
    }

instance Functor MyProcessor where
    fmap f m = MyProcessor $ \s ->
        let (a, s') = step m s
        in (f a, s')

instance Applicative MyProcessor where
    pure a = MyProcessor (a, )
    mf <*> mg = MyProcessor $ \s ->
        let (h, s') = step mf s
            (a, s'') = step mg s'
        in (h a, s'')

instance Monad MyProcessor where
    m >>= f = MyProcessor $ \s ->
        let (a, s') = step m s
        in step (f a) s'

myget :: MyProcessor ParseState
myget = MyProcessor $ \s -> (s, s)

myput :: ParseState -> MyProcessor ()
myput s = MyProcessor $ const ((), s)

mymodify :: (ParseState -> ParseState) -> MyProcessor ()
mymodify f = do
    s <- myget
    myput $ f s

------------------------------------

data Token = Token
    deriving (Show, Eq)

mylex :: MyProcessor Token
mylex = do
    ParseState { steps, remain } <- myget
    myput ParseState { steps = steps + 1, remain }
    return Token

------------------------------------

data Event
    = Shift
    | Reduce
    | Accept
    | Error
    deriving (Show, Eq)

myparse :: Token -> MyProcessor Event
myparse _ = do
    ParseState { steps, remain } <- myget
    myput ParseState { steps = steps + 1, remain }
    return Accept

------------------------------------

data CST = CST
    deriving (Show, Eq)

process :: MyProcessor (Either CST ())
process = do
    token <- mylex
    event <- myparse token
    case event of
        Accept -> return $ Left CST
        _ -> return $ Right ()

runMyProcessor :: MyProcessor (Either CST ()) -> String -> CST
runMyProcessor m input =
    let run s =
            let (a, s') = step m s
            in case a of
                Left cst -> cst
                Right _ -> run s'
        s0 = ParseState { steps = 0, remain = input }
    in run s0

------------------------------------

normal :: String -> CST
normal = runMyProcessor process

withErrorHandling :: String -> CST
withErrorHandling = runMyProcessor process
