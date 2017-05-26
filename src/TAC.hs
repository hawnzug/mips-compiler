{-# LANGUAGE OverloadedStrings #-}
module TAC where

import           Control.Arrow
import           Control.Monad.State
import qualified Data.Map            as M
import qualified Data.Text           as T
import           Syntax

data Assignee = Num Integer
              | Variable LValue
              deriving (Eq)

instance Show Assignee where
    show (Num x)       = show x
    show (Variable lv) = show lv

data LValue = Act Name
            | Tmp Int
            deriving (Eq)

instance Show LValue where
    show (Act name) = T.unpack name
    show (Tmp i)    = "_t" ++ show i

data Label = Label Int
    deriving (Eq)

instance Show Label where
    show (Label x) = "_L" ++ show x

data IR = Iassign LValue Assignee
        | Iadd LValue Assignee Assignee
        | Isub LValue Assignee Assignee
        | Ieql LValue Assignee Assignee
        | Ineq LValue Assignee Assignee
        | Ijmp Label
        | Iifz Assignee Label Label
        | Iload Name Assignee
        | Isave Name Assignee
        | Ilabel Label
        deriving (Eq)

instance Show IR where
    show (Iassign lv ass) = "\t" ++ show lv ++ " = " ++ show ass
    show (Iadd lv a1 a2)  = "\t" ++ show lv ++ " = " ++ show a1 ++ " + " ++ show a2
    show (Isub lv a1 a2)  = "\t" ++ show lv ++ " = " ++ show a1 ++ " - " ++ show a2
    show (Ieql lv a1 a2)  = "\t" ++ show lv ++ " = " ++ show a1 ++ " == " ++ show a2
    show (Ineq lv a1 a2)  = "\t" ++ show lv ++ " = " ++ show a1 ++ " != " ++ show a2
    show (Ijmp label)     = "\t" ++ "GOTO " ++ show label
    show (Iifz v jmp fall)   = "\t" ++ "IFZ " ++ show v ++ " GOTO " ++ show jmp ++ " ELSE " ++ show fall
    show (Iload name a)   = "\t" ++ "LOAD " ++ show name ++ " " ++ show a
    show (Isave name a)   = "\t" ++ "SAVE " ++ show name ++ " " ++ show a
    show (Ilabel label)   = show label ++ ":\tNOP"

transBlk :: Block -> State ([IR], Int) ()
transBlk = mapM_ one

one :: Statement -> State ([IR], Int) ()
one (VarDecl _) = return ()
one (Assign name expr) = do
    putIRs irs
    putIRs [Iassign (Act name) ret]
        where (ret, irs) = gen expr

one (If (IfIf cond block)) = do
    elseLabel <- fuckHlint cond
    transBlk block
    putIRs [Ilabel elseLabel]

one (If (IfEl cond ifblk elseblk)) = do
    elseLabel <- fuckHlint cond
    transBlk ifblk
    jmpLabel <- genLabel
    putIRs [Ijmp jmpLabel, Ilabel elseLabel]
    transBlk elseblk
    putIRs [Ilabel jmpLabel]
        where (cond_name, pre) = gen cond

one (Load name expr) = do
    putIRs irs
    putIRs [Iload name ret]
        where (ret, irs) = gen expr

fuckHlint cond = do
    putIRs pre
    trueLabel <- genLabel
    falseLabel <- genLabel
    putIRs [Iifz cond_name falseLabel trueLabel, Ilabel trueLabel]
    return falseLabel
        where (cond_name, pre) = gen cond

gen :: Expr -> (Assignee, [IR])
gen expr = second fst $ runState (gexpr expr) ([], 0)

gexpr :: Expr -> State ([IR], Int) Assignee
gexpr (Var name)  = return (Variable (Act name))
gexpr (Const x)   = return (Num x)
gexpr (Add e1 e2) = binop Iadd e1 e2
gexpr (Sub e1 e2) = binop Isub e1 e2
gexpr (Eql e1 e2) = binop Ieql e1 e2
gexpr (Neq e1 e2) = binop Ineq e1 e2

binop constr e1 e2 = do
    name1 <- gexpr e1
    name2 <- gexpr e2
    tmp <- genTemp
    putIRs [constr tmp name1 name2]
    return (Variable tmp)

genTemp :: State ([IR], Int) LValue
genTemp = fmap Tmp genSuc

genLabel :: State ([IR], Int) Label
genLabel = fmap Label genSuc

genSuc :: State ([IR], Int) Int
genSuc = do
    (_, n) <- get
    modify (second (+1))
    return n

putIRs :: [IR] -> State ([IR], Int) ()
putIRs irs = modify (first (++ irs))

trans :: [IR] -> [IR]
trans irs = removeRep map irs
    where map = labelSubst (False, 0) irs M.empty

type LabelMap = M.Map Int Int

removeRep :: LabelMap -> [IR] -> [IR]
removeRep _ [] = []
removeRep m (instr@(Ilabel (Label x)) : rest) = if M.member x m then removeRep m rest else instr : removeRep m rest
removeRep m (instr@(Ijmp (Label x)) : rest) =
    case M.lookup x m of
      Just n  -> Ijmp (Label n)
      Nothing -> instr
    : removeRep m rest
removeRep m (instr@(Iifz a (Label x) (Label y)) : rest) =
    case (M.lookup x m, M.lookup y m) of
      (Just n1, Just n2) -> Iifz a (Label n1) (Label n2)
      (Just n1, Nothing) -> Iifz a (Label n1) (Label y)
      (Nothing, Just n2) -> Iifz a (Label x)  (Label n2)
      (Nothing, Nothing) -> instr
    : removeRep m rest
removeRep m (instr : rest) = instr : removeRep m rest

labelSubst :: (Bool, Int) -> [IR] -> LabelMap -> LabelMap
labelSubst _ [] m = m
labelSubst (True, n) (Ilabel (Label x) : rest) m = labelSubst (True, n) rest (M.insert x n m)
labelSubst (False, _) (Ilabel (Label n) : rest) m = labelSubst (True, n) rest m
labelSubst _ (_ : rest) m = labelSubst (False, 0) rest m

ast2tac :: Block -> [IR]
ast2tac prog = trans . fst . snd  $ runState (transBlk prog) ([], 0)
