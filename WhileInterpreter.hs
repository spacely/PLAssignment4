

{-# LANGUAGE FlexibleContexts #-}   

module Main where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Combinator
-- import Text.Parsec.Token as Token
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import Text.ParserCombinators.Parsec.Language(haskellStyle, haskellDef, reservedNames, reservedOpNames)
import Data.Functor
import Data.Function (on)
import qualified Data.Map as Map
-- import qualified Data.Map.Lazy as MapL
import Data.List --(intercalate, intersperse)
import Control.Applicative  hiding ((<|>))
import System.Environment
import Control.Monad


type Storage = Map.Map String Integer

-- generated based on online resources from https://wiki.haskell.org/Parsing_a_simple_imperative_language
-- https://wiki.haskell.org/wikiupload/c/c6/ICMI45-paper-en.pdf
-- https://dokuwiki.unica.it/doku.php?id=sem_op_fun

data Aexp
    = IntExp Integer        
    | VarExp String
    | SumExp Aexp Aexp
    | SubExp Aexp Aexp
    | MulExp Aexp Aexp
    | NegExp Aexp
    
data Bexp
    = BoolExp Bool          -- From While slides in class.
    | EqExp Aexp Aexp
    | LtExp Aexp Aexp
    | NotExp Bexp
    | AndExp Bexp Bexp
    | OrExp Bexp Bexp

data Cmd
    = Skip 
    | List [Cmd]
    | AssignExp Aexp Aexp
    | AssignNegExp Aexp Aexp
    | SeqExp Cmd Cmd 
    | IfExp Bexp Cmd Cmd
    | WhileExp Bexp Cmd

lexer :: Token.TokenParser()
lexer = Token.makeTokenParser 
    (haskellDef{
        Token.reservedOpNames = [
            "¬",
            "+",
            "-",
            "*",
            "<",
            ":=",
            "∨",
            "∧",
            "=",
            ":=-",
            ":"
       ],
        Token.reservedNames = [
            "if",
            "then",
            "else",
            "do",
            "while",
            "true",
            "false",
            "skip",
            "nop"
       ]
    })

identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
braces = Token.braces lexer
integer = Token.integer lexer
semi = Token.semi lexer
whiteSpace = Token.whiteSpace lexer

aOperators = [
    [Prefix (NegExp <$ reservedOp "-")],
    [Infix (MulExp <$ reservedOp "*") AssocLeft],
    [Infix (SumExp <$ reservedOp "+") AssocLeft,
    Infix (SubExp <$ reservedOp "-") AssocLeft]]

bOperators = [
    [Prefix (NotExp <$ reservedOp "¬")],
    [Infix (AndExp <$ reservedOp "∧") AssocLeft],
    [Infix (OrExp <$ reservedOp "∨" ) AssocLeft]] 
  

aExpr = buildExpressionParser aOperators aTerm
    where
        aTerm = parens aExpr
            <|> VarExp <$> identifier
            <|> IntExp <$> integer
            
bExpr = buildExpressionParser bOperators bTerm
    where
        bTerm = parens bExpr
            <|> try (NotExp <$ reservedOp "¬" *> bExpr)
            <|> (BoolExp True <$ reserved "true")
            <|> (BoolExp False <$ reserved "false")
            <|> try (EqExp <$> (aExpr <* reservedOp "=") <*> aExpr)   
            <|> try (LtExp <$> (aExpr <* reservedOp "<") <*> aExpr)
            <|> try (AndExp <$> (bExpr <* reservedOp "∧") <*> bExpr)
            <|> (OrExp <$> (bExpr <* reservedOp "∨") <*> bExpr)

binary s f assoc = Infix (f <$ string s) assoc
prefix s f = Prefix (f <$ string s)
-- factor = (between `on` char) '(' ')' expr <|> (IntExp . read <$> many1 digit)



whileParser :: Parser Cmd
whileParser = whiteSpace >> statementParser

statementParser :: Parser Cmd
statementParser = braces statementParser 
            <|> List <$> sepBy statement semi

statement :: Parser Cmd
statement = parens statement
    <|> try (Skip <$ reserved "skip") 
    <|> try (AssignExp <$> (VarExp <$> identifier) 
                        <*> (reservedOp ":=" *> aExpr))
    <|> try (AssignNegExp <$> (VarExp <$> identifier) 
                        <*> (reservedOp ":=-" *> aExpr))
    <|> try (WhileExp <$> (reserved "while" *> bExpr <* reserved "do") 
                        <*>   statementParser)
    <|> try (IfExp <$> (reserved "if" *> bExpr <* reserved "then") 
                <*>   statementParser 
                <*> (reserved "else" *> statementParser))
    <|> (IfExp <$> (bExpr <* reservedOp "?") 
                <*> (statementParser) 
                <*> (reservedOp ":" *> statementParser))
    -- <|> (SeqExp)


-- Eval Functions --
-- Eval Arith
evalAexp :: Aexp -> Storage -> Integer
evalAexp (IntExp n)  _ = n
evalAexp (NegExp e1) s = negate $ (evalAexp e1) s
evalAexp (VarExp e1) s = Map.findWithDefault 0 e1 s
evalAexp (SumExp e1 e2) s = (evalAexp e1) s + (evalAexp e2) s
evalAexp (SubExp e1 e2) s = (evalAexp e1) s - (evalAexp e2) s
evalAexp (MulExp e1 e2) s = (evalAexp e1) s * (evalAexp e2) s

-- Eval Boolean
evalBexp :: Bexp -> Storage -> Bool
evalBexp (BoolExp e1) _ = e1
evalBexp (EqExp e1 e2) s = (evalAexp e1) s == (evalAexp e2) s
evalBexp (LtExp e1 e2) s = (evalAexp e1) s < (evalAexp e2) s
evalBexp (NotExp e1) s = not (evalBexp e1 s)
evalBexp (AndExp e1 e2) s = (evalBexp e1 s) && (evalBexp e2 s)
evalBexp (OrExp e1 e2) s = (evalBexp e1 s) || (evalBexp e2 s)

-- Eval Commands
evalCmd :: Cmd -> Storage -> Storage
evalCmd Skip s = s
evalCmd (AssignExp (VarExp e1) e2) s = Map.insert e1 (evalAexp e2 s) s
evalCmd (AssignNegExp (VarExp e1) e2) s = Map.insert e1 (negate $ evalAexp e2 s) s
evalCmd (List[]) s = s
evalCmd (List (e1:e2)) s = (evalCmd (List e2) (evalCmd e1 s))
evalCmd (SeqExp e1 e2) s = (evalCmd e2 (evalCmd e1 s))
evalCmd (IfExp e1 e2 e3) s
    | (evalBexp e1) s = (evalCmd e2) s
    | otherwise = (evalCmd e3 s)
evalCmd (WhileExp e1 e2) s
    | (evalBexp e1) s = (evalCmd (WhileExp e1 e2) (evalCmd e2 s))
    | otherwise = s

output :: Cmd -> Storage
output e1 = (evalCmd e1 Map.empty)


-- converting to list for print
addval [] (v, k) = [(v ++ " → " ++ (show k))]
addval ls (v, k) = ((v ++ " → " ++ (show k)) : ls)

--addval [] (k, v) = [(k ++ " → " ++ (show v))]
--addval ls (k, v) = ((k ++ " → " ++ (show v)) : ls)
convert ls = foldl addval [] ls


main:: IO()
main = do

    line <- getContents
    putStr "{"
    case parse whileParser "" line of
        Left e -> print e >> fail "Parse Error"
        Right ast -> putStr $ intercalate ", " $ convert (Map.toList (output ast))
    putStr "}"    
