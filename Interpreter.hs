module Interpreter where 

import Lexer 
import Parser

isValue :: Expr -> Bool 
isValue BTrue = True 
isValue BFalse = True
isValue (Num _) = True 
isValue (Lam _ _ _) = True
isValue (Stack _) = True
isValue _ = False 

subst :: String -> Expr -> Expr -> Expr 
subst x n (Var v) = if (x == v) then
                      n 
                    else 
                      (Var v)
subst x n (Lam v t b) = Lam v t (subst x n b)
subst x n (App e1 e2) = App (subst x n e1) (subst x n e2)
subst x n (Add e1 e2) = Add (subst x n e1) (subst x n e2)
subst x n (And e1 e2) = And (subst x n e1) (subst x n e2)
subst x n (If e1 e2 e3) = If (subst x n e1) (subst x n e2) (subst x n e3)
subst x n (Paren e) = Paren (subst x n e)
subst x n (Let v e1 e2) = Let v (subst x n e1) (subst x n e2)
subst x n (Or e1 e2) = Or (subst x n e1) (subst x n e2)
subst x n (Sub e1 e2) = Sub (subst x n e1) (subst x n e2)
subst x n (Mult e1 e2) = Mult (subst x n e1) (subst x n e2)
subst x n (MoreThan e1 e2) = MoreThan (subst x n e1) (subst x n e2)
subst x n (LessThan e1 e2) = LessThan (subst x n e1) (subst x n e2)
subst x n (EqualTo e1 e2) = EqualTo (subst x n e1) (subst x n e2)
subst x n (MoreThanOrEqualTo e1 e2) = MoreThanOrEqualTo (subst x n e1) (subst x n e2)
subst x n (LessThanOrEqualTo e1 e2) = LessThanOrEqualTo (subst x n e1) (subst x n e2)
subst x n (CBracket e) = CBracket (subst x n e)
subst x n (StackPop e1 e2) = StackPop e1 (subst x n e2)
subst x n (StackPush e1 e2) = StackPush e1 (subst x n e2)
subst x n (StackTop e1) = StackTop e1
subst x n (StackSize e1 e2) = StackSize e1 (subst x n e2)
subst x n e = e 

step :: Expr -> Expr 
step (Add (Num n1) (Num n2)) = Num (n1 + n2)
step (Add (Num n) e) = Add (Num n) (step e)
step (Add e1 e2) = Add (step e1) e2 

step (And BFalse _) = BFalse 
step (And BTrue e) = e 
step (And e1 e2) = And (step e1) e2 

step (If BFalse e1 e2) = e2 
step (If BTrue e1 e2) = e1 
step (If e e1 e2) = If (step e) e1 e2 

step (Paren e) = e

step (App (Lam x t b) e2) | isValue e2 = subst x e2 b 
                        | otherwise = (App (Lam x t b) (step e2))

step (App e1 e2) = App (step e1) e2

step (Let v e1 e2) | isValue e1 = subst v e1 e2 
                   | otherwise = Let v (step e1) e2

step (Or BTrue _) = BTrue
step (Or BFalse e) = e
step (Or e1  e2) = Or (step e1) e2

step (Sub (Num n1) (Num n2)) = Num (n1 - n2)
step (Sub (Num n) e) = Sub (Num n) (step e)
step (Sub e1 e2) = Sub (step e1) e2

step (Mult (Num n1) (Num n2)) = Num (n1*n2)
step (Mult (Num n) e) = Mult (Num n) (step e)
step (Mult e1 e2) = Mult (step e1) e2

step (MoreThan (Num n1) (Num n2)) = if n1 > n2 then
                                      BTrue
                                    else
                                      BFalse
step (MoreThan (Num n) e) = MoreThan (Num n) (step e)
step (MoreThan e1 e2) = MoreThan (step e1) e2

step (LessThan (Num n1) (Num n2)) = if n1 < n2 then
                                      BTrue
                                    else
                                      BFalse
step (LessThan (Num n) e) = LessThan (Num n) (step e)
step (LessThan e1 e2) = LessThan (step e1) e2

step (EqualTo (Num n1) (Num n2)) = if n1 == n2 then
                                      BTrue
                                    else
                                      BFalse
step (EqualTo (Num n) e) = EqualTo (Num n) (step e)
step (EqualTo e1 e2) = EqualTo (step e1) e2

step (MoreThanOrEqualTo (Num n1) (Num n2)) = if n1 >= n2 then
                                              BTrue
                                             else
                                              BFalse
step (MoreThanOrEqualTo (Num n) e) = MoreThanOrEqualTo (Num n) (step e)
step (MoreThanOrEqualTo e1 e2) = MoreThanOrEqualTo (step e1) e2

step (LessThanOrEqualTo (Num n1) (Num n2)) = if n1 <= n2 then
                                                BTrue
                                             else
                                                BFalse
step (LessThanOrEqualTo (Num n) e) = LessThanOrEqualTo (Num n) (step e)
step (LessThanOrEqualTo e1 e2) = LessThanOrEqualTo (step e1) e2

step (StackPop list (Stack list2)) = if (null (tail list)) then
                              Stack list2
                            else 
                              step(StackPop (tail list) (Stack (list2 ++ [head list])))
              
step (StackPush list (Num n)) = Stack (list ++ [Num n])
step (StackPush list BTrue) = Stack (list ++ [BTrue])
step (StackPush list BFalse) = Stack (list ++ [BFalse])

step (StackTop list) = if (null (tail list)) then
                          (head(list))
                       else
                        step(StackTop (tail list))

step (StackSize list (Num n)) = if (null (tail list)) then
                              Num (n+1)
                            else
                              step(StackSize (tail list) (Num (n+1)))

step (CBracket e) = e

step e = error (show e)

eval :: Expr -> Expr 
eval e | isValue e = e 
       | otherwise = eval (step e)