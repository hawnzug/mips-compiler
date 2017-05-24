module Temp where

import Control.Monad.Writer

one :: Int -> Writer [Int] Int
one x = do
    tell [3]
    tell [5]
    return x

good :: Writer [Int] Int
good = do
    x <- one 0
    y <- one (x+1)
    return (y + 1)
