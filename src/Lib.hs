module Lib
    ( someFunc
    ) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment
import Parser (parseProg)
import TAC (ast2tac)
import TAC2IR (tacToir, seeProc, tacToblk)
import ConstProp (optTest)
import Compiler.Hoopl

someFunc :: IO ()
someFunc = do
    [f] <- getArgs
    a <- T.readFile f
    case parseProg a of
      Left err -> print err
      Right prog -> let graph = tacToir $ ast2tac prog in
                        seeProc graph >>
                        (print . runSimpleUniqueMonad . runWithFuel 999 . optTest . fmap snd) graph
