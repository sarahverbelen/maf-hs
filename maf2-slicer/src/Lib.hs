module Lib
    ( someFunc
    ) where

import Dependency.Lattice
import Lattice

someFunc :: IO ()
someFunc = putStrLn $ show $ refine ZeroOrNeg