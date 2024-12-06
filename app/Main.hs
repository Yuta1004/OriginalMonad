import MyMaybe
import MyEither
import MyContainer

import MyState
import MyProcessor

main :: IO ()
main = do
    print runMyMaybe
    print runMyResult
    print runMyContainer
    print $ execMyState runMyState 0
    print $ execMyState runMyState' 0
    print $ execMyState runMyState'' 0
    print $ normal "1 + 2"
    print $ withErrorHandling "1 + 2"
