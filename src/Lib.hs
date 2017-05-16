module Lib
    ( someFunc
    ) where

import qualified Data.Text as T
import Parser (parseProg)
import Control.Monad (forever)

someFunc :: IO ()
someFunc = forever $ do
  putStr "> "
  a <- getLine
  print $ parseProg (T.pack a)
