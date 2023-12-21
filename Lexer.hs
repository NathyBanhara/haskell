module Lexer where 

import Data.Char 

data Expr = BTrue
          | BFalse 
          | Num Int 
          | Add Expr Expr 
          | And Expr Expr 
          | If Expr Expr Expr
          | Var String
          | Lam String Ty Expr 
          | App Expr Expr
          | Paren Expr
          | Let String Expr Expr
          | Sub Expr Expr
          | Mult Expr Expr
          | Or Expr Expr
          | MoreThan Expr Expr
          | LessThan Expr Expr
          | EqualTo Expr Expr
          | MoreThanOrEqualTo Expr Expr
          | LessThanOrEqualTo Expr Expr
          | StackPop [Expr] Expr
          | StackPush [Expr] Expr
          | StackTop [Expr]
          | StackSize [Expr] Expr
          | CBracket Expr
          | Stack [Expr]
          deriving Show

data Ty = TBool 
        | TNum 
        | TFun Ty Ty
        deriving (Show, Eq)

data Token = TokenTrue 
           | TokenFalse
           | TokenNum Int 
           | TokenAdd
           | TokenAnd 
           | TokenIf 
           | TokenThen 
           | TokenElse
           | TokenVar String 
           | TokenLam
           | TokenArrow
           | TokenLParen
           | TokenRParen
           | TokenLet 
           | TokenEq 
           | TokenIn
           | TokenColon
           | TokenBoolean 
           | TokenNumber
           | TokenSub
           | TokenSubt
           | TokenMult
           | TokenOr
           | TokenMoreThan
           | TokenLessThan
           | TokenEqualTo
           | TokenMoreThanOrEqualTo
           | TokenLessThanOrEqualTo
           | TokenStackPop
           | TokenStackPush
           | TokenStackTop
           | TokenStackSize
           | TokenComma
           | TokenLCBracket
           | TokenRCBracket
           | TokenDot
           deriving (Show, Eq)

isSymb :: Char -> Bool 
isSymb c = c `elem` "+&\\->()=:|*<,{}."

lexer :: String -> [Token]
lexer [] = [] 
lexer ('(':cs) = TokenLParen : lexer cs
lexer (')':cs) = TokenRParen : lexer cs
lexer ('{':cs) = TokenLCBracket : lexer cs
lexer ('}':cs) = TokenRCBracket : lexer cs
lexer (c:cs) | isSpace c = lexer cs 
             | isDigit c = lexNum (c:cs)
             | isSymb c = lexSymbol (c:cs)
             | isAlpha c = lexKW (c:cs)
lexer _ = error "Lexical error!"

lexNum :: String -> [Token]
lexNum cs = case span isDigit cs of 
              (num, rest) -> TokenNum (read num) : lexer rest

lexSymbol :: String -> [Token]
lexSymbol cs = case span isSymb cs of 
                 ("+", rest)  -> TokenAdd : lexer rest 
                 ("&&", rest) -> TokenAnd : lexer rest 
                 ("\\", rest) -> TokenLam : lexer rest 
                 ("->", rest) -> TokenArrow : lexer rest 
                 ("=", rest)  -> TokenEq : lexer rest 
                 (":", rest)  -> TokenColon : lexer rest
                 ("||", rest) -> TokenOr : lexer rest
                 ("-", rest) -> TokenSubt : lexer rest
                 ("*", rest) -> TokenMult : lexer rest
                 (">", rest) -> TokenMoreThan : lexer rest
                 ("<", rest) -> TokenLessThan : lexer rest
                 ("==", rest) -> TokenEqualTo : lexer rest
                 (">=", rest) -> TokenMoreThanOrEqualTo : lexer rest
                 ("<=", rest) -> TokenLessThanOrEqualTo : lexer rest
                 (",", rest) -> TokenComma : lexer rest
                 (".", rest) -> TokenDot : lexer rest
                 _ -> error "Lexical error: invalid symbol!"

lexKW :: String -> [Token]
lexKW cs = case span isAlpha cs of 
             ("true", rest) -> TokenTrue : lexer rest 
             ("false", rest) -> TokenFalse : lexer rest
             ("if", rest) -> TokenIf : lexer rest 
             ("then", rest) -> TokenThen : lexer rest 
             ("else", rest) -> TokenElse : lexer rest 
             ("let", rest) -> TokenLet : lexer rest 
             ("in", rest) -> TokenIn : lexer rest 
             ("Num", rest) -> TokenNumber : lexer rest 
             ("Bool", rest) -> TokenBoolean : lexer rest 
             ("pop", rest) -> TokenStackPop : lexer rest
             ("push", rest) -> TokenStackPush : lexer rest
             ("top", rest) -> TokenStackTop : lexer rest
             ("size", rest) -> TokenStackSize : lexer rest
             (var, rest) -> TokenVar var : lexer rest 


