module Examples where

import Import


andExpr,orExpr,notExpr,expr1,expr2,expr3 :: BoolExp Char
andExpr = And (Lit 'a') (Lit 'b')
orExpr = Or (Lit 'a') (Lit 'b') 
notExpr = Not (Lit 'a')
expr1 = (Or (And (Lit 'a') (Lit 'b')) (Lit 'c'))
expr2 = (Or (Or (Lit 'a') (Lit 'b')) (Lit 'c'))
expr3 = (And (And (Lit 'a') (Lit 'b')) (Not (Lit 'c')))
