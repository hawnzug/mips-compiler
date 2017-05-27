{-# LANGUAGE GADTs #-}
module IR where

import Compiler.Hoopl
import qualified Syntax as S

type M = CheckingFuelMonad SimpleUniqueMonad

data Proc = Proc { entry :: Label, body :: Graph Insn C C }

instance Show Proc where
    show proc = showGraph show (body proc)

data Insn e x where
  Label  :: Label      -> Insn C O
  Assign :: S.LValue   -> S.Assignee -> Insn O O
  Add    :: S.LValue   -> S.Assignee -> S.Assignee -> Insn O O
  Sub    :: S.LValue   -> S.Assignee -> S.Assignee -> Insn O O
  Eql    :: S.LValue   -> S.Assignee -> S.Assignee -> Insn O O
  Neq    :: S.LValue   -> S.Assignee -> S.Assignee -> Insn O O
  Jump   :: Label      -> Insn O C
  Cond   :: S.Assignee -> Label      -> Label      -> Insn O C
  Save   :: S.Name     -> S.Assignee -> Insn O O
  Load   :: S.Name     -> S.Assignee -> Insn O O
  Return :: Insn O C

instance NonLocal Insn where
  entryLabel (Label l)    = l
  successors (Jump l)     = [l]
  successors (Cond _ t f) = [t, f]
  successors Return       = []

instance Show (Insn e x) where
    show (Label label)     = show label ++ ":\tNOP"
    show (Assign lv ass)   = "\t" ++ show lv ++ " = " ++ show ass
    show (Add lv a1 a2)    = "\t" ++ show lv ++ " = " ++ show a1 ++ " + " ++ show a2
    show (Sub lv a1 a2)    = "\t" ++ show lv ++ " = " ++ show a1 ++ " - " ++ show a2
    show (Eql lv a1 a2)    = "\t" ++ show lv ++ " = " ++ show a1 ++ " == " ++ show a2
    show (Neq lv a1 a2)    = "\t" ++ show lv ++ " = " ++ show a1 ++ " != " ++ show a2
    show (Jump label)      = "\t" ++ "GOTO " ++ show label
    show (Cond v jmp fall) = "\t" ++ "IFZ " ++ show v ++ " GOTO " ++ show jmp ++ " ELSE " ++ show fall
    show (Load name a)     = "\t" ++ "LOAD " ++ show name ++ " " ++ show a
    show (Save name a)     = "\t" ++ "SAVE " ++ show name ++ " " ++ show a
    show Return            = "\t" ++ "RETURN"
