{
module Parser where

import Lexer 
}

%name parser 
%tokentype { Token }
%error { parserError } 

%left '+'

%token 
    num         { TokenNum $$ }
    '+'         { TokenAdd }
    "&&"        { TokenAnd }
    true        { TokenTrue }
    false       { TokenFalse }
    if          { TokenIf }
    then        { TokenThen }
    else        { TokenElse }
    var         { TokenVar $$ }
    '\\'        { TokenLam }
    "->"        { TokenArrow }
    '('         { TokenLParen }
    ')'         { TokenRParen }
    '='         { TokenEq }
    let         { TokenLet }
    in          { TokenIn }
    Bool        { TokenBoolean }
    Num         { TokenNumber }
    ':'         { TokenColon }
    '-'         { TokenSubt }
    '*'         { TokenMult }
    "||"        { TokenOr }
    ">"         { TokenMoreThan }
    "<"         { TokenLessThan }
    "=="        { TokenEqualTo }
    ">="        { TokenMoreThanOrEqualTo }
    "<="        { TokenLessThanOrEqualTo }
    '.'         { TokenDot }
    '{'         { TokenLCBracket }
    '}'         { TokenRCBracket }
    ','         { TokenComma }
    "pop"       { TokenStackPop }
    "push"      { TokenStackPush }
    "top"       { TokenStackTop }
    "size"      { TokenStackSize }

%%

Exp         : num                                            { Num $1 }
            | true                                           { BTrue }
            | false                                          { BFalse }
            | Exp '+' Exp                                    { Add $1 $3 }
            | Exp "&&" Exp                                   { And $1 $3 }
            | if Exp then Exp else Exp                       { If $2 $4 $6 }
            | var                                            { Var $1 }
            | '\\' var ':' Type "->" Exp                     { Lam $2 $4 $6 }
            | Exp Exp                                        { App $1 $2 }
            | '(' Exp ')'                                    { Paren $2 }
            | let var '=' Exp in Exp                         { Let $2 $4 $6 }
            | Exp '-' Exp                                    { Sub $1 $3 }
            | Exp '*' Exp                                    { Mult $1 $3 }
            | Exp "||" Exp                                   { Or $1 $3 }
            | Exp ">" Exp                                    { MoreThan $1 $3 }
            | Exp "<" Exp                                    { LessThan $1 $3 }
            | Exp "==" Exp                                   { EqualTo $1 $3 }
            | Exp ">=" Exp                                   { MoreThanOrEqualTo $1 $3 }
            | Exp "<=" Exp                                   { LessThanOrEqualTo $1 $3 }
            | '{' Exp ',' Exp '}' '.' "pop" '(' ')'          { StackPop [$2, $4] (Stack []) }
            | '{' Exp ',' Exp '}' '.' "push" '(' Exp ')'     { StackPush [$2, $4] $9 }
            | '{' Exp ',' Exp '}' '.' "top" '(' ')'          { StackTop [$2, $4] }
            | '{' Exp ',' Exp '}' '.' "size" '(' ')'          { StackSize [$2, $4] (Num 0) }

Type    : Bool                              { TBool }
        | Num                               { TNum }
        | '(' Type "->" Type ')'            { TFun $2 $4 }

{

parserError :: [Token] -> a 
parserError _ = error "Syntax error!"

}