{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE ScopedTypeVariables, GADTs #-}
module ConstProp (constPropPass) where

import qualified Data.Map as Map

import Compiler.Hoopl hiding ((<*>))
import Syntax(Assignee(..), LValue(..))
import Data.Maybe (fromMaybe)
import IR

type Node = Insn
type Lit = Integer
type Var = LValue

type ConstFact = Map.Map Var (WithTop Lit)
constLattice :: DataflowLattice ConstFact
constLattice = DataflowLattice
    { fact_name = "Const var value"
  , fact_bot  = Map.empty
  , fact_join = joinMaps (extendJoinDomain constFactAdd) }
      where
          constFactAdd _ (OldFact old) (NewFact new) =
            if new == old
               then (NoChange, PElem new)
               else (SomeChange, Top)

varHasLit :: FwdTransfer Node ConstFact
varHasLit = mkFTransfer ft
    where
  ft :: Node e x -> ConstFact -> Fact x ConstFact
  ft (Label _) f          = f
  ft (Assign x (Num k)) f = Map.insert x (PElem k) f
  ft (Assign x _) f       = Map.insert x Top f
  ft (Save _ _) f         = f
  ft (Load x _) f         = Map.insert (Act x) Top f
  ft (Jump l) f           = mapSingleton l f
  ft (Cond _ tl fl) f     = mkFactBase constLattice [(tl, f), (fl, f)]
  ft Return _             = mapEmpty
  ft Add{} f              = f
  ft Sub{} f              = f
  ft Eql{} f              = f
  ft Neq{} f              = f

insnToG :: Insn e x -> Graph Insn e x
insnToG n@(Label _)    = mkFirst n
insnToG n@(Assign _ _) = mkMiddle n
insnToG n@Add{}        = mkMiddle n
insnToG n@Sub{}        = mkMiddle n
insnToG n@Eql{}        = mkMiddle n
insnToG n@Neq{}        = mkMiddle n
insnToG n@(Save _ _)   = mkMiddle n
insnToG n@(Load _ _)   = mkMiddle n
insnToG n@(Jump _)     = mkLast n
insnToG n@Cond{}       = mkLast n
insnToG Return         = mkLast Return

type MaybeChange a = a -> Maybe a
constProp :: forall m. FuelMonad m => FwdRewrite m Node ConstFact
constProp = mkFRewrite cp
    where
    cp :: Node e x -> ConstFact -> m (Maybe (Graph Node e x))
    cp node f = return $ insnToG <$> mapVN (lookup f) node

    mapVN :: (Var -> Maybe Assignee) -> MaybeChange (Node e x)
    mapVN _ (Label _) = Nothing
    mapVN f (Assign name (Variable v)) = fmap (Assign name) (f v)
    mapVN _ Assign{} = Nothing
    mapVN f (Add name a1 a2) = detect f (Add name) a1 a2
    mapVN f (Sub name a1 a2) = detect f (Sub name) a1 a2
    mapVN f (Eql name a1 a2) = detect f (Eql name) a1 a2
    mapVN f (Neq name a1 a2) = detect f (Neq name) a1 a2
    mapVN _ (Jump _)  = Nothing
    mapVN f (Save name (Variable v)) = case f v of
                                         Just n -> Just (Save name n)
                                         Nothing -> Nothing
    mapVN f (Load name (Variable v)) = case f v of
                                         Just n -> Just (Load name n)
                                         Nothing -> Nothing
    mapVN f (Cond (Variable v) l1 l2) = case f v of
                                          Just n -> Just (Cond n l1 l2)
                                          Nothing -> Nothing
    mapVN _ Cond{} = Nothing
    mapVN _ Save{} = Nothing
    mapVN _ Load{} = Nothing
    mapVN _ Return  = Nothing

    detect f constr e1 e2 =
        case (help e1, help e2) of
          (Nothing, Nothing) -> Nothing
          (v1, v2) -> Just $ constr (fromMaybe e1 v1) (fromMaybe e2 v2)
        where help (Num _) = Nothing
              help (Variable v) = f v

    lookup :: ConstFact -> Var -> Maybe Assignee
    lookup f x = case Map.lookup x f of
                   Just (PElem n) -> Just $ Num n
                   _              -> Nothing

simplify :: forall m f. FuelMonad m => FwdRewrite m Node f
simplify = deepFwdRw simp
 where
  simp :: forall e x. Node e x -> f -> m (Maybe (Graph Node e x))
  simp node _ = return $ insnToG <$> s_node node

  s_node :: Node e x -> Maybe (Node e x)
  s_node (Cond (Num n) t f) = Just $ Jump (if n == 0 then t else f)
  s_node (Add name (Num n1) (Num n2)) = Just $ Assign name $ Num $ n1 + n2
  s_node (Sub name (Num n1) (Num n2)) = Just $ Assign name $ Num $ n1 - n2
  s_node (Eql name (Num n1) (Num n2)) = Just $ Assign name $ Num $ if n1 == n2 then 1 else 0
  s_node (Neq name (Num n1) (Num n2)) = Just $ Assign name $ Num $ if n1 /= n2 then 1 else 0
  s_node _ = Nothing

constPropPass :: FuelMonad m => FwdPass m Insn ConstFact
constPropPass = FwdPass
    { fp_lattice = constLattice
    , fp_transfer = varHasLit
    , fp_rewrite = constProp `thenFwdRw` simplify }
