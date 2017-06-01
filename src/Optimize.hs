module Optimize where

import IR(M, Proc(..))
import ConstProp
import Live

import Compiler.Hoopl

optimize :: M Proc -> M Proc
optimize mproc = do
    proc@Proc { entry=entry0, body=body0 } <- mproc
    (body1, _, _) <- analyzeAndRewriteFwd constPropPass (JustC [entry0]) body0 mapEmpty
    (body2, _, _) <- analyzeAndRewriteBwd livenessPass  (JustC [entry0]) body1 mapEmpty
    return $ proc { body = body2 }
