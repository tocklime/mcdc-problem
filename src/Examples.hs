-- | This module contains some expressions which are useful when playing around in the repl.
module Examples where

import           Expressions

andExpr, orExpr, notExpr, expr1, expr2, expr3, aAndNotA, twoAs, twoAs2 ::
     BoolExp Char
-- | A && B
andExpr = And (Var 'a') (Var 'b')

-- | A || B
orExpr = Or (Var 'a') (Var 'b')

-- | !A
notExpr = Not (Var 'a')

-- | (A && B) || C
expr1 = Or (And (Var 'a') (Var 'b')) (Var 'c')

-- | (A || B) && C
expr2 = Or (Or (Var 'a') (Var 'b')) (Var 'c')

-- | (A && B) && C
expr3 = And (And (Var 'a') (Var 'b')) (Not (Var 'c'))

-- | (A && !A)
aAndNotA = And (Var 'a') (Not (Var 'a'))

-- | A && A
twoAs = Or (And (Var 'a') (Var 'b')) (And (Not (Var 'a')) (Var 'c'))

-- | (A && B) || (A && C)
twoAs2 = Or (And (Var 'a') (Var 'b')) (And (Var 'a') (Var 'c'))
