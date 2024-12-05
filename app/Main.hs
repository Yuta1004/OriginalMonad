import MyMaybe
import MyEither
import MyContainer

import MyState

main :: IO ()
main = do
    print runMyMaybe
    print runMyResult
    print runMyContainer
    print $ execMyState runMyState 0
