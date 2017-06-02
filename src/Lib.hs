module Lib
    ( someFunc
    ) where

import qualified Data.Text.IO as T
import System.Environment
import Parser (parseProg)
import TAC (ast2tac)
import TAC2IR (tacToir, seeProc)
import Optimize
import Compiler.Hoopl

someFunc :: IO ()
someFunc = do
    [f] <- getArgs
    a <- T.readFile f
    case parseProg a of
      Left err -> print err
      Right prog -> let graph = tacToir $ ast2tac prog
                        proc = (runSimpleUniqueMonad . runWithFuel 999 . optimize . fmap snd) graph
                     in do seeProc graph
                           print proc
