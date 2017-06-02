{-# LANGUAGE ScopedTypeVariables, GADTs #-}
module EmptyBlock (emptyBlockPass) where

import Compiler.Hoopl
import Data.Maybe (fromMaybe)
import IR

type EmptyBlock = Maybe Label
emptyBlockLattice :: DataflowLattice EmptyBlock
emptyBlockLattice = DataflowLattice
    { fact_name = "EmptyBlock"
    , fact_bot  = Nothing
    , fact_join = discard
    }
        where discard _ _ (NewFact new) = (SomeChange, new)

emptyTrans :: BwdTransfer Insn EmptyBlock
emptyTrans = mkBTransfer empt
    where
        empt :: Insn e x -> Fact x EmptyBlock -> EmptyBlock
        empt (Label _) fact = fact
        empt (Jump label) fact = Just $ case lookupFact label fact of
                                          Just (Just old) -> old
                                          _ -> label
        empt _ _ = fact_bot emptyBlockLattice

emptyBlockElim :: forall m . FuelMonad m => BwdRewrite m Insn EmptyBlock
emptyBlockElim = mkBRewrite d
    where
        d :: Insn e x -> Fact x EmptyBlock -> m (Maybe (Graph Insn e x))
        d (Jump label) fact = return $ case lookupFact label fact of
                                         Just (Just new) -> Just $ mkLast $ Jump new
                                         _ -> Nothing
        d (Cond name zl nzl) fact = return $ case (look zl, look nzl) of
                                               (Nothing, Nothing) -> Nothing
                                               (n1, n2) -> Just $ mkLast (Cond name (fromMaybe zl n1) (fromMaybe nzl n2))
            where look label = case lookupFact label fact of
                                 Just (Just new) -> Just new
                                 _ -> Nothing
        d _ _ = return Nothing

emptyBlockPass :: FuelMonad m => BwdPass m Insn EmptyBlock
emptyBlockPass = BwdPass
    { bp_lattice  = emptyBlockLattice
    , bp_transfer = emptyTrans
    , bp_rewrite  = emptyBlockElim }
