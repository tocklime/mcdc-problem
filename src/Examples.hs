-- | This module contains some expressions which are useful when playing around in the repl.
module Examples where

import           Expressions

andExpr, orExpr, notExpr, expr1, expr2, expr3, aAndNotA, twoAs, twoAs2 ::
     BoolExp Char
-- | A && B
andExpr = And (Lit 'a') (Lit 'b')

-- | A || B
orExpr = Or (Lit 'a') (Lit 'b')

-- | !A
notExpr = Not (Lit 'a')

-- | (A && B) || C
expr1 = Or (And (Lit 'a') (Lit 'b')) (Lit 'c')

-- | (A || B) && C
expr2 = Or (Or (Lit 'a') (Lit 'b')) (Lit 'c')

-- | (A && B) && C
expr3 = And (And (Lit 'a') (Lit 'b')) (Not (Lit 'c'))

-- | (A && !A)
aAndNotA = And (Lit 'a') (Not (Lit 'a'))

-- | A && A
twoAs = Or (And (Lit 'a') (Lit 'b')) (And (Not (Lit 'a')) (Lit 'c'))

twoAs2 = Or (And (Lit 'a') (Lit 'b')) (And (Lit 'a') (Lit 'c'))
