{-# LANGUAGE OverloadedStrings #-}
module TAC where

import           Control.Arrow
import           Control.Monad.State
import           Syntax

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
    putIRs [Ijmp elseLabel, Ilabel elseLabel]

one (If (IfEl cond ifblk elseblk)) = do
    elseLabel <- fuckHlint cond
    transBlk ifblk
    jmpLabel <- genLabel
    putIRs [Ijmp jmpLabel, Ilabel elseLabel]
    transBlk elseblk
    putIRs [Ijmp jmpLabel, Ilabel jmpLabel]

one (Load name expr) = do
    putIRs irs
    putIRs [Iload name ret]
        where (ret, irs) = gen expr

one (Save name expr) = do
    putIRs irs
    putIRs [Isave name ret]
        where (ret, irs) = gen expr

fuckHlint :: Expr -> State ([IR], Int) Label
fuckHlint cond = do
    putIRs pre
    trueLabel <- genLabel
    falseLabel <- genLabel
    putIRs [Iifz cond_name falseLabel trueLabel, Ilabel trueLabel]
    return falseLabel
        where (cond_name, pre) = gen cond

gen :: Expr -> (Assignee, [IR])
gen expr = second fst $ runState (gexpr expr) ([], 0)
    where
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

ast2tac :: Block -> [IR]
ast2tac prog = (fst . snd  $ runState (transBlk prog) ([Ilabel (Label 0)], 1)) ++ [Ireturn]
