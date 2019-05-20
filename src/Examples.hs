-- | This module contains some expressions which are useful when playing around in the repl.
module Examples where

import           Expressions

andExpr, orExpr, notExpr, expr1, expr2, expr3, aAndNotA, twoAs, twoAs2 :: BoolExp Char
andExpr = And (Lit 'a') (Lit 'b')

orExpr = Or (Lit 'a') (Lit 'b')

notExpr = Not (Lit 'a')

expr1 = Or (And (Lit 'a') (Lit 'b')) (Lit 'c')

expr2 = Or (Or (Lit 'a') (Lit 'b')) (Lit 'c')

expr3 = And (And (Lit 'a') (Lit 'b')) (Not (Lit 'c'))

aAndNotA = And (Lit 'a') (Not (Lit 'a'))

twoAs = Or (And (Lit 'a') (Lit 'b')) (And (Not (Lit 'a')) (Lit 'c'))

twoAs2 = Or (And (Lit 'a') (Lit 'b')) (And (Lit 'a') (Lit 'c'))
