{-# LANGUAGE GADTs #-}
module Optimize where

import IR(M, Proc(..), Insn(..))
import ConstProp
import Live
import EmptyBlock
import JoinBlock

import Compiler.Hoopl

optimize :: M Proc -> M Proc
optimize mproc = do
    proc@Proc { entry=entry0, body=body0 } <- mproc
    (body1, _, _) <- analyzeAndRewriteFwd constPropPass  (JustC [entry0]) body0 mapEmpty
    (body2, _, _) <- analyzeAndRewriteBwd livenessPass   (JustC [entry0]) body1 mapEmpty
    (body3, _, _) <- analyzeAndRewriteBwd emptyBlockPass (JustC [entry0]) body2 mapEmpty
    (body4, _, _) <- analyzeAndRewriteFwd justFwdPass    (JustC [entry0]) body3 mapEmpty
    return $ proc { body = joinBlock body4 }

type NoFact = ()
noLattice :: DataflowLattice NoFact
noLattice = DataflowLattice
    { fact_name = "Empty Lattice"
    , fact_bot  = ()
    , fact_join = \_ _ _ -> (NoChange, ())}

noTrans :: FwdTransfer Insn NoFact
noTrans = mkFTransfer3 (const2 ()) (const2 ()) temp
    where const2 x _ _ = x
          temp :: Insn O C -> NoFact -> FactBase NoFact
          temp (Cond _ tl fl) _ = mkFactBase noLattice [(tl, ()), (fl, ())]
          temp (Jump l) _ = mkFactBase noLattice [(l, ())]
          temp Return _ = noFacts

justFwdPass :: FuelMonad m => FwdPass m Insn NoFact
justFwdPass = FwdPass noLattice noTrans noFwdRewrite
