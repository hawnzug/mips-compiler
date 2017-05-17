{-# LANGUAGE OverloadedStrings #-}
module Syntax where

import qualified Data.Text as T

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
