module Main where

import MyMaybe
import MyEither

main :: IO ()
main = do
    print runMyMaybe
    print runMyResult
