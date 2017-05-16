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

data IR = Iassign LValue Assignee
        | Iadd LValue Assignee Assignee
        | Isub LValue Assignee Assignee
        | Ieql LValue Assignee Assignee
        | Ineq LValue Assignee Assignee
        | Ijmp Label
        | Iifz LValue Label
        deriving (Eq)

instance Show IR where
    show (Iassign lv ass) = show lv ++ " = " ++ show ass
    show (Iadd lv a1 a2) = show lv ++ " = " ++ show a1 ++ " + " ++ show a2
    show (Isub lv a1 a2) = show lv ++ " = " ++ show a1 ++ " - " ++ show a2
    show (Ieql lv a1 a2) = show lv ++ " = " ++ show a1 ++ " == " ++ show a2
    show (Ineq lv a1 a2) = show lv ++ " = " ++ show a1 ++ " != " ++ show a2

generate :: Block -> [IR]
generate = reverse . concatMap one

one :: Statement -> [IR]
one (VarDecl _) = []
one (Assign name expr) = Iassign (Act name) ret : irs
    where (ret, irs, _) = gen expr 0

gen :: Expr -> Int -> (Assignee, [IR], Int)
gen (Var name) n = (Variable (Act name), [], n)
gen (Const x) n = (Num x, [], n)
gen (Add e1 e2) n = binop Iadd e1 e2 n
gen (Sub e1 e2) n = binop Isub e1 e2 n
gen (Eql e1 e2) n = binop Ieql e1 e2 n
gen (Neq e1 e2) n = binop Ineq e1 e2 n

binop constr e1 e2 n = (Variable (Tmp n2),
                        constr (Tmp n2) v1 v2 : irs2 ++ irs1,
                        n2 + 1)
    where (v1, irs1, n1) = gen e1 n
          (v2, irs2, n2) = gen e2 n1

fk (_, irs, _) = reverse irs
