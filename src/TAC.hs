{-# LANGUAGE OverloadedStrings #-}
module TAC where

import Syntax
import qualified Data.Text as T

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

data TacIR = Labeled Label IR
           | NoLabel IR
           deriving (Eq)

instance Show TacIR where
    show (Labeled label ir) = show label ++ ":  " ++ show ir
    show (NoLabel ir) = "      " ++ show ir

data IR = Iassign LValue Assignee
        | Iadd LValue Assignee Assignee
        | Isub LValue Assignee Assignee
        | Ieql LValue Assignee Assignee
        | Ineq LValue Assignee Assignee
        | Ijmp Label
        | Iifz Assignee Label
        | Iload Name Assignee
        | Isave Name Assignee
        | Inop
        deriving (Eq)

instance Show IR where
    show (Iassign lv ass) = show lv ++ " = " ++ show ass
    show (Iadd lv a1 a2) = show lv ++ " = " ++ show a1 ++ " + " ++ show a2
    show (Isub lv a1 a2) = show lv ++ " = " ++ show a1 ++ " - " ++ show a2
    show (Ieql lv a1 a2) = show lv ++ " = " ++ show a1 ++ " == " ++ show a2
    show (Ineq lv a1 a2) = show lv ++ " = " ++ show a1 ++ " != " ++ show a2
    show (Ijmp label) = "GOTO " ++ show label
    show (Iifz v label) = "IFZ " ++ show v ++ " GOTO " ++ show label
    show (Iload name a) = "LOAD " ++ show name ++ " " ++ show a
    show (Isave name a) = "SAVE " ++ show name ++ " " ++ show a
    show Inop = "NOP"

loop :: Block -> Int -> ([TacIR], Int)
loop [] n = ([], n)
loop (x:xs) n = (irs ++ res, n2)
    where (irs, n1) = one x n
          (res, n2) = loop xs n1

one :: Statement -> Int -> ([TacIR], Int)
one (VarDecl _) n = ([], n)
one (Assign name expr) n = (map NoLabel (irs ++ [Iassign (Act name) ret]), n)
    where (ret, irs, _) = gen expr 0

one (If (IfIf cond block)) n = (map NoLabel pre ++ ifz ++ blk ++ after, n1 + 1)
    where (v_cond, pre, _) = gen cond 0
          ifz = [NoLabel $ Iifz v_cond label]
          (blk, n1) = loop block n
          after = [Labeled label Inop]
          label = Label n1

one (If (IfEl cond ifblk elseblk)) n = (map NoLabel pre ++ ifz ++ ifb ++ middle ++ elb ++ after, n2 + 2)
    where (v_cond, pre, _) = gen cond 0
          ifz = [NoLabel $ Iifz v_cond label0]
          (ifb, n1) = loop ifblk n
          (elb, n2) = loop elseblk n1
          middle = [NoLabel (Ijmp label1), Labeled label0 Inop]
          after = [Labeled label1 Inop]
          label0 = Label n2
          label1 = Label (n2 + 1)

one (Load name expr) n = (map NoLabel (irs ++ [Iload name ret]), n)
    where (ret, irs, _) = gen expr 0


gen :: Expr -> Int -> (Assignee, [IR], Int)
gen (Var name) n = (Variable (Act name), [], n)
gen (Const x) n = (Num x, [], n)
gen (Add e1 e2) n = binop Iadd e1 e2 n
gen (Sub e1 e2) n = binop Isub e1 e2 n
gen (Eql e1 e2) n = binop Ieql e1 e2 n
gen (Neq e1 e2) n = binop Ineq e1 e2 n

binop constr e1 e2 n = (Variable (Tmp n2),
                        irs1 ++ irs2 ++ [constr (Tmp n2) v1 v2],
                        n2 + 1)
    where (v1, irs1, n1) = gen e1 n
          (v2, irs2, n2) = gen e2 n1

fk (_, irs, _) = reverse irs

fuck block = mapM_ print . fst $ loop block 0
