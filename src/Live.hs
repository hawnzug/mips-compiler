{-# OPTIONS_GHC -Wall -fno-warn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables, GADTs #-}
module Live (livenessPass) where

import Data.Maybe
import qualified Data.Set as S

import Compiler.Hoopl
import IR
import Syntax(Assignee(..), LValue(..))

type Var = LValue

type Live = S.Set Var
liveLattice :: DataflowLattice Live
liveLattice = DataflowLattice
  { fact_name = "Live variables"
  , fact_bot  = S.empty
  , fact_join = add
  }
    where add _ (OldFact old) (NewFact new) = (ch, j)
            where
              j = new `S.union` old
              ch = changeIf (S.size j > S.size old)

liveness :: BwdTransfer Insn Live
liveness = mkBTransfer live
  where
    live :: Insn e x -> Fact x Live -> Live
    live (Label _) f = f
    live (Assign x e) f = add1del1 x e f
    live (Add left e1 e2) f = add2del1 left e1 e2 f
    live (Sub left e1 e2) f = add2del1 left e1 e2 f
    live (Eql left e1 e2) f = add2del1 left e1 e2 f
    live (Neq left e1 e2) f = add2del1 left e1 e2 f
    live (Jump l) f = fact f l
    live (Cond e tl fl) f = addVar (fact f tl `S.union` fact f fl) e
    live (Save name e) f = add2 (Variable $ Act name) e f
    live (Load name e) f = add1del1 (Act name) e f
    live Return _ = fact_bot liveLattice

    add2 e1 e2 f = addVar (addVar f e1) e2
    add1del1 x e f = S.delete x (addVar f e)
    add2del1 left e1 e2 f = S.delete left $ add2 e1 e2 f

    fact :: FactBase (S.Set Var) -> Label -> Live
    fact f l = fromMaybe S.empty $ lookupFact l f

    addVar s (Variable v) = S.insert v s
    addVar s _       = s

deadAsstElim :: forall m . FuelMonad m => BwdRewrite m Insn Live
deadAsstElim = mkBRewrite d
  where
    d :: Insn e x -> Fact x Live -> m (Maybe (Graph Insn e x))
    d (Assign x _) = help x
    d (Load x _) = help (Act x)
    d (Add x _ _) = help x
    d (Sub x _ _) = help x
    d (Eql x _ _) = help x
    d (Neq x _ _) = help x
    d _ = const $ return Nothing

    help x live = return $ if not (x `S.member` live) then Just emptyGraph else Nothing

livenessPass :: FuelMonad m => BwdPass m Insn Live
livenessPass = BwdPass { bp_lattice  = liveLattice
                       , bp_transfer = liveness
                       , bp_rewrite  = deadAsstElim }
