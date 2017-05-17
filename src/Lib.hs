module Lib
    ( someFunc
    ) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment
import Parser (parseProg)
import TAC (fuck)

someFunc :: IO ()
someFunc = do
    [f] <- getArgs
    a <- T.readFile f
    case parseProg a of
      Left err -> print err
      Right prog -> fuck prog
