{-# LANGUAGE OverloadedStrings #-}
module TAC where

import Syntax
import qualified Data.Text as T
import qualified Data.Map as M
import Control.Monad.Writer
import Control.Monad.State
import Control.Arrow

data Assignee = Num Integer
              | Variable LValue
              deriving (Eq)

instance Show Assignee where
    show (Num x) = show x
    show (Variable lv) = show lv

data LValue = Act Name
            | Tmp Int
            deriving (Eq)

instance Show LValue where
    show (Act name) = T.unpack name
    show (Tmp i) = "_t" ++ show i

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
        | Iifz Assignee Label
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
    show (Iifz v label)   = "\t" ++ "IFZ " ++ show v ++ " GOTO " ++ show label
    show (Iload name a)   = "\t" ++ "LOAD " ++ show name ++ " " ++ show a
    show (Isave name a)   = "\t" ++ "SAVE " ++ show name ++ " " ++ show a
    show (Ilabel label)   = show label ++ ":\tNOP"

transBlk :: Block -> Int -> ([IR], Int)
transBlk [] n = ([], n)
transBlk (x:xs) n = (irs ++ res, n2)
    where (irs, n1) = one x n
          (res, n2) = transBlk xs n1

one :: Statement -> Int -> ([IR], Int)
one (VarDecl _) n = ([], n)
one (Assign name expr) n = (irs ++ [Iassign (Act name) ret], n)
    where (ret, irs) = gen expr

one (If (IfIf cond block)) n = (pre ++ ifz ++ blk ++ after, n1 + 1)
    where (v_cond, pre) = gen cond
          ifz = [Iifz v_cond label]
          (blk, n1) = transBlk block n
          after = [Ilabel label]
          label = Label n1

one (If (IfEl cond ifblk elseblk)) n = (pre ++ ifz ++ ifb ++ middle ++ elb ++ after, n2 + 2)
    where (v_cond, pre) = gen cond
          ifz = [Iifz v_cond label0]
          (ifb, n1) = transBlk ifblk n
          (elb, n2) = transBlk elseblk n1
          middle = [Ijmp label1, Ilabel label0]
          after = [Ilabel label1]
          label0 = Label n2
          label1 = Label (n2 + 1)

one (Load name expr) n = (irs ++ [Iload name ret], n)
    where (ret, irs) = gen expr

gen :: Expr -> (Assignee, [IR])
gen expr = second fst $ runState (gexpr expr) ([], 0)

gexpr :: Expr -> State ([IR], Int) Assignee
gexpr (Var name) = return (Variable (Act name))
gexpr (Const x) = return (Num x)
gexpr (Add e1 e2) = binop Iadd e1 e2
gexpr (Sub e1 e2) = binop Isub e1 e2
gexpr (Eql e1 e2) = binop Ieql e1 e2
gexpr (Neq e1 e2) = binop Ineq e1 e2

binop constr e1 e2 = do
    name1 <- gexpr e1
    name2 <- gexpr e2
    (_, tmp) <- get
    putIR (constr (Tmp tmp) name1 name2)
    genTemp
    return (Variable (Tmp tmp))

genTemp :: State ([IR], Int) ()
genTemp = modify (second (+1))

putIR :: IR -> State ([IR], Int) ()
putIR ir = modify (first (++ [ir]))

trans :: [IR] -> [IR]
trans irs = removeRep map irs
    where map = labelSubst (False, 0) irs M.empty

type LabelMap = M.Map Int Int

removeRep :: LabelMap -> [IR] -> [IR]
removeRep _ [] = []
removeRep m (instr@(Ilabel (Label x)) : rest) = if M.member x m then removeRep m rest else instr : removeRep m rest
removeRep m (instr@(Ijmp (Label x)) : rest) =
    case M.lookup x m of
      Just n -> Ijmp (Label n) : removeRep m rest
      Nothing -> instr : removeRep m rest
removeRep m (instr@(Iifz a (Label x)) : rest) =
    case M.lookup x m of
      Just n -> Iifz a (Label n) : removeRep m rest
      Nothing -> instr : removeRep m rest
removeRep m (instr : rest) = instr : removeRep m rest

labelSubst :: (Bool, Int) -> [IR] -> LabelMap -> LabelMap
labelSubst _ [] m = m
labelSubst (True, n) (Ilabel (Label x) : rest) m = labelSubst (True, n) rest (M.insert x n m)
labelSubst (False, _) (Ilabel (Label n) : rest) m = labelSubst (True, n) rest m
labelSubst _ (_ : rest) m = labelSubst (False, 0) rest m

ast2tac :: Block -> [IR]
ast2tac prog = trans . fst $ transBlk prog 0
