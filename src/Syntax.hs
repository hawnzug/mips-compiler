{-# LANGUAGE OverloadedStrings #-}
module Syntax where

import qualified Data.Text as T

-- AST

type Block = [Statement]

data Statement = VarDecl Name
               | Assign Name Expr
               | If IfStmt
               | Load Name Expr
               | Save Name Expr
               deriving (Show, Eq)

data IfStmt = IfIf Expr Block
            | IfEl Expr Block Block
    deriving (Show, Eq)

data Expr = Var Name
          | Const Integer
          | Add Expr Expr
          | Sub Expr Expr
          | Eql Expr Expr
          | Neq Expr Expr
          deriving (Show, Eq)

type Name = T.Text

-- TAC

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

newtype Label = Label Int
    deriving (Eq, Ord)

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
        | Ireturn
        deriving (Eq)

instance Show IR where
    show (Iassign lv ass)  = "\t" ++ show lv ++ " = " ++ show ass
    show (Iadd lv a1 a2)   = "\t" ++ show lv ++ " = " ++ show a1 ++ " + " ++ show a2
    show (Isub lv a1 a2)   = "\t" ++ show lv ++ " = " ++ show a1 ++ " - " ++ show a2
    show (Ieql lv a1 a2)   = "\t" ++ show lv ++ " = " ++ show a1 ++ " == " ++ show a2
    show (Ineq lv a1 a2)   = "\t" ++ show lv ++ " = " ++ show a1 ++ " != " ++ show a2
    show (Ijmp label)      = "\t" ++ "GOTO " ++ show label
    show (Iifz v jmp fall) = "\t" ++ "IFZ " ++ show v ++ " GOTO " ++ show jmp ++ " ELSE " ++ show fall
    show (Iload name a)    = "\t" ++ "LOAD " ++ show name ++ " " ++ show a
    show (Isave name a)    = "\t" ++ "SAVE " ++ show name ++ " " ++ show a
    show (Ilabel label)    = show label ++ ":\tNOP"
    show Ireturn           = "\t" ++ "RETURN"
