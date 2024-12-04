module Main where

import MyMaybe
import MyEither
import MyContainer

main :: IO ()
main = do
    print runMyMaybe
    print runMyResult
    print runMyContainer
