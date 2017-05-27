module Lib
    ( someFunc
    ) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment
import Parser (parseProg)
import TAC (ast2tac)
import TAC2IR (tacToir, seeProc, tacToblk)

someFunc :: IO ()
someFunc = do
    [f] <- getArgs
    a <- T.readFile f
    case parseProg a of
      Left err -> print err
      Right prog -> (seeProc . tacToir . ast2tac) prog
