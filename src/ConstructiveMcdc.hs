module ConstructiveMcdc where

import           Data.Either.Combinators (mapLeft)
import           Data.Set                (Set)
import qualified Data.Set                as S
import           Expressions
import           TruthVectors
import           ValueAssignment

-- | A useful 'Maybe' to 'Either e' mapper, using the given constant value when a 'Left' is needed.
(<?>) :: Maybe a -> e -> Either e a
Just a <?> _ = Right a
Nothing <?> e = Left e

-- | Possible errors from joining 'ValueAssignment's
data JoinError a
  = NoSingleElement
  | ValueConflict (Set a)
  | NoMcdcCoverage (ValueAssignment a)
  deriving (Show)

-- | Join 2 'ValueAssignment's using 'and' logic: Take all 'TruthVector's from
-- 'a' and merge them with a true value from 'b' (because true is the identity of '&&')
-- Take all 'TruthVector's from 'b' and merge them with a true value from 'a'.
-- Reassemble the items into their new true and false value sets and return the new
-- 'TruthVector'.
joinAnd ::
     (Ord a)
  => ValueAssignment a
  -> ValueAssignment a
  -> Either (JoinError a) (ValueAssignment a)
joinAnd a b
    -- needs intelligence to select which single element each time
    -- otherwise (AB | (!A)C) is unsolvable.
 = do
  aA <- S.lookupMin (trues a) <?> NoSingleElement
  aB <- S.lookupMin (trues b) <?> NoSingleElement
  asWithB1 <- mapLeft ValueConflict $ joinVals (trues a) aB
  asWithB2 <- mapLeft ValueConflict $ joinVals (falses a) aB
  bsWithA1 <- mapLeft ValueConflict $ joinVals (trues b) aA
  bsWithA2 <- mapLeft ValueConflict $ joinVals (falses b) aA
  let xx = S.union asWithB1 bsWithA1
  let xy = S.union asWithB2 bsWithA2
  return $ TruthVector xy xx

-- | Same as 'joinAnd', but for or. Implemented using the equivalence "A || B == !(!A && !B)"
joinOr ::
     (Ord a)
  => ValueAssignment a
  -> ValueAssignment a
  -> Either (JoinError a) (ValueAssignment a)
joinOr a b = invert <$> joinAnd (invert a) (invert b)

-- | BoolExp evaluator which gives a MCDC minimal set for a given expression.
findMcdc :: Ord a => BoolExp a -> Either (JoinError a) (ValueAssignment a)
findMcdc =
  monadEval (Evaluator (pure . invert) joinAnd joinOr (Right . singleton))
