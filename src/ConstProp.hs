{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE ScopedTypeVariables, GADTs #-}
module ConstProp (optTest) where

import qualified Data.Map as Map

import Compiler.Hoopl
import Syntax(Assignee(..), LValue)
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

initFact :: [Var] -> ConstFact
initFact vars = Map.fromList [(v, Top) | v <- vars]

varHasLit :: FwdTransfer Node ConstFact
varHasLit = mkFTransfer ft
    where
  ft :: Node e x -> ConstFact -> Fact x ConstFact
  ft (Label _)            f = f
  ft (Assign x (Num k))   f = Map.insert x (PElem k) f
  ft (Assign x _)         f = Map.insert x Top f
  ft (Save _ _)           f = f
  ft (Load _ _)           f = f
  ft (Jump l)             f = mapSingleton l f
  ft (Cond (Variable x) tl fl) f
    = mkFactBase constLattice
           [(tl, Map.insert x (PElem 0)  f),
            (fl, Map.insert x (PElem 0) f)]
  ft (Cond _ tl fl) f
    = mkFactBase constLattice [(tl, f), (fl, f)]
  ft Return              _ = mapEmpty
  ft (Add x (Num n1) (Num n2)) f = Map.insert x (PElem (n1 + n2)) f
  ft (Sub x (Num n1) (Num n2)) f = Map.insert x (PElem (n1 - n2)) f
  ft (Eql x (Num n1) (Num n2)) f = Map.insert x (PElem (if n1 == n2 then 1 else 0)) f
  ft (Neq x (Num n1) (Num n2)) f = Map.insert x (PElem (if n1 == n2 then 0 else 1)) f
  ft Add{} f = f
  ft Sub{} f = f
  ft Eql{} f = f
  ft Neq{} f = f

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
    mapVN f (Add name a1 a2) = detect f Add (+) name a1 a2
    mapVN f (Sub name a1 a2) = detect f Sub (-) name a1 a2
    mapVN f (Eql name a1 a2) = detect f Eql (\x y -> if x == y then 1 else 0) name a1 a2
    mapVN f (Neq name a1 a2) = detect f Neq (\x y -> if x /= y then 1 else 0) name a1 a2
    mapVN _ (Jump _)  = Nothing
    mapVN f (Save name (Variable v))  = case f v of
                                          Just n -> Just (Save name n)
                                          Nothing -> Nothing
    mapVN f (Load name (Variable v))  = case f v of
                                          Just n -> Just (Load name n)
                                          Nothing -> Nothing
    mapVN f (Cond (Variable v) l1 l2) = case f v of
                                          Just n -> Just (Cond n l1 l2)
                                          Nothing -> Nothing
    mapVN _ Cond{} = Nothing
    mapVN _ Save{} = Nothing
    mapVN _ Load{} = Nothing
    mapVN _ Return  = Nothing

    detect f constr op name (Variable v1) (Variable v2) =
        case (f v1, f v2) of
          (Just (Num n1), Just (Num n2)) -> Just (Assign name (Num (op n1 n2)))
          (Just n1, Nothing) -> Just (constr name n1 (Variable v2))
          (Nothing, Just n2) -> Just (constr name (Variable v1) n2)
          _ -> Nothing
    detect f _ op name (Variable v) (Num n2) =
        case f v of
          Just (Num n1) -> Just (Assign name (Num (op n1 n2)))
          _ -> Nothing
    detect f _ op name (Num n1) (Variable v) =
        case f v of
          Just (Num n2) -> Just (Assign name (Num (op n1 n2)))
          _ -> Nothing
    detect _ _ op name (Num n1) (Num n2) = Just (Assign name (Num (op n1 n2)))

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
  s_node (Cond (Num n) t f) = Just $ Jump (if n == 1 then t else f)
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

optTest :: M Proc -> M Proc
optTest aproc = aproc >>= optProc
  where
    optProc proc@Proc {entry=entry, body=body} =
      do { (body',  _, _) <- analyzeAndRewriteFwd fwd (JustC [entry]) body
                             (mapSingleton entry (initFact []))
         ; return $ proc { body = body' } }
    fwd  = constPropPass
