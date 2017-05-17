{-# LANGUAGE OverloadedStrings #-}
module Parser where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Functor.Identity (Identity)
import Text.Parsec
import Text.Parsec.Text
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Language as Lang

import Syntax

lexer :: Tok.GenTokenParser T.Text () Identity
lexer = Tok.makeTokenParser style

style :: Tok.GenLanguageDef T.Text () Identity
style = Lang.emptyDef
    { Tok.commentStart    = "/*"
    , Tok.commentEnd      = "*/"
    , Tok.commentLine     = "//"
    , Tok.nestedComments  = True
    , Tok.identStart      = letter
    , Tok.identLetter     = alphaNum <|> oneOf "_'"
    , Tok.opStart         = Tok.opLetter style
    , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , Tok.reservedOpNames = ["+", "-", "==", "!="]
    , Tok.reservedNames   = ["let", "load", "save", "if", "else"]
    , Tok.caseSensitive   = True
    }

parens :: Parser a -> Parser a
parens = Tok.parens lexer

reservedOp :: T.Text -> Parser ()
reservedOp op = Tok.reservedOp lexer (T.unpack op)

reserved :: T.Text -> Parser ()
reserved name = Tok.reserved lexer (T.unpack name)

ident :: Parser T.Text
ident = T.pack <$> Tok.identifier lexer

natural :: Parser Integer
natural = Tok.natural lexer

var :: Parser Expr
var = do
    var <- ident
    return (Var var)

constant :: Parser Expr
constant = do
    nat <- natural
    return (Const nat)

expr :: Parser Expr
expr = term `chainl1` cmpop

term :: Parser Expr
term = factor `chainl1` addop

factor :: Parser Expr
factor = constant <|> var <|> parens expr

infixOp :: T.Text -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp x f = reservedOp x >> return f

cmpop :: Parser (Expr -> Expr -> Expr)
cmpop = infixOp "==" Eql <|> infixOp "!=" Neq

addop :: Parser (Expr -> Expr -> Expr)
addop = infixOp "+" Add <|> infixOp "-" Sub

block :: Parser Block
block = many statement

statement :: Parser Statement
statement = decl <|> ifs <|> load <|> save <|> assign

ifs :: Parser Statement
ifs = do
    s <- ifstmt
    return (If s)

ifstmt :: Parser IfStmt
ifstmt = do
    reserved "if"
    e <- expr
    reserved "{"
    b <- block
    reserved "}"
    (do
       eb <- ifelse <|> elseif
       return (IfEl e b eb))
     <|>
     return (IfIf e b)

ifelse :: Parser Block
ifelse = do
    try $ do
        reserved "else"
        reserved "{"
    b <- block
    reserved "}"
    return b

elseif :: Parser Block
elseif = do
    try $ reserved "else"
    is <- ifstmt
    return [If is]

decl :: Parser Statement
decl = do
    reserved "let"
    var <- ident
    reserved ";"
    return (VarDecl var)

assign :: Parser Statement
assign = do
    var <- ident
    reservedOp "="
    e <- expr
    reserved ";"
    return (Assign var e)

load :: Parser Statement
load = memory "load" Load

save :: Parser Statement
save = memory "save" Save

memory s c = do
    reserved s
    var <- ident
    e <- expr
    reserved ";"
    return (c var e)

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

parseProg :: T.Text -> Either ParseError Block
parseProg = parse (contents block) "<stdin>"
