module Main (main) where

import Lib


main :: IO ()
main = do
    testLabeling
    putStrLn ""
    testSlicer 1
