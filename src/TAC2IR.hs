{-# LANGUAGE ExplicitForAll #-}
module TAC2IR where

import           Compiler.Hoopl hiding ((<*>))
import qualified Compiler.Hoopl as H ((<*>))
import           Control.Monad
import           Control.Applicative as AP (Applicative(..))
import qualified Data.Map       as M

import qualified IR             as I
import qualified Syntax         as S

type IdLabelMap = M.Map S.Label Label
newtype LabelMapM a = LabelMapM (IdLabelMap -> I.M (IdLabelMap, a))

instance Monad LabelMapM where
  return = AP.pure
  LabelMapM f1 >>= k = LabelMapM (\m -> do (m', x) <- f1 m
                                           let (LabelMapM f2) = k x
                                           f2 m')

instance Functor LabelMapM where
  fmap = liftM

instance Applicative LabelMapM where
  pure x = LabelMapM (\m -> return (m, x))
  (<*>) = ap

labelFor :: S.Label -> LabelMapM Label
getBody  :: forall n. Graph n C C   -> LabelMapM (Graph n C C)
run      :: LabelMapM a -> I.M (IdLabelMap, a)

data Part = Part { first :: S.Label, mids :: [S.IR], control :: S.IR }
    deriving (Show, Eq)

seeProc :: I.M (IdLabelMap, I.Proc) -> IO ()
seeProc m = print $ snd (runSimpleUniqueMonad $ runWithFuel 0 m)

tacToir :: [S.IR] -> I.M (IdLabelMap, I.Proc)
tacToir = blkToIR . tacToblk

tacToblk :: [S.IR] -> [Part]
tacToblk [] = []
tacToblk (S.Ilabel label : irs) = Part { first = label, mids = init this, control = last this} : tacToblk rem
    where (this, rem) = break isLabel irs

blkToIR :: [Part] -> I.M (IdLabelMap, I.Proc)
blkToIR b = run $
  do entry <- getEntry b
     body  <- toBody   b
     return I.Proc { I.body = body, I.entry = entry }

getEntry :: [Part] -> LabelMapM Label
getEntry [] = error "Parsed procedures should not be empty"
getEntry (b : _) = labelFor $ first b

toBody :: [Part] -> LabelMapM (Graph I.Insn C C)
toBody bs =
  do g <- foldl (liftM2 (|*><*|)) (return emptyClosedGraph) (map toBlock bs)
     getBody g

toBlock :: Part -> LabelMapM (Graph I.Insn C C)
toBlock Part { first = f, mids = ms, control = l } =
  do f'  <- toFirst f
     ms' <- mapM toMid ms
     l'  <- toLast l
     return $ mkFirst f' H.<*> mkMiddles ms' H.<*> mkLast l'

toFirst :: S.Label -> LabelMapM (I.Insn C O)
toFirst = fmap I.Label . labelFor

toMid :: S.IR -> LabelMapM (I.Insn O O)
toMid (S.Iassign r a) = return $ I.Assign r a
toMid (S.Iload r a) = return $ I.Load r a
toMid (S.Isave r a) = return $ I.Save r a
toMid (S.Iadd r e1 e2) = return $ I.Add r e1 e2
toMid (S.Isub r e1 e2) = return $ I.Sub r e1 e2
toMid (S.Ieql r e1 e2) = return $ I.Eql r e1 e2
toMid (S.Ineq r e1 e2) = return $ I.Neq r e1 e2

toLast :: S.IR -> LabelMapM (I.Insn O C)
toLast (S.Ijmp l) = I.Jump <$> labelFor l
toLast (S.Iifz e t f) = do
    t' <- labelFor t
    f' <- labelFor f
    return (I.Cond e t' f')
toLast S.Ireturn = return I.Return

isLabel :: S.IR -> Bool
isLabel (S.Ilabel _) = True
isLabel _ = False

labelFor l = LabelMapM f
  where f m = case M.lookup l m of
                Just l' -> return (m, l')
                Nothing -> do l' <- freshLabel
                              let m' = M.insert l l' m
                              return (m', l')

getBody graph = LabelMapM f
  where f m = return (m, graph)

run (LabelMapM f) = f M.empty -- >>=  return -- . snd
