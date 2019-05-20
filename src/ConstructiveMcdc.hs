module ConstructiveMcdc where

import           Data.Either.Combinators (mapLeft)
import           Data.Set                (Set)
import qualified Data.Set                as S
import           Expressions
import           McdcCheck
import           TruthVectors
import           ValueAssignment
-- | A useful Maybe to Eitehr e mapper, using the given constant value when a Left is needed.
(<?>) :: Maybe a -> e -> Either e a
Just a <?> _ = Right a
Nothing <?> e = Left e

-- | Possible errors from joining ValueAssignments
data JoinError a
  = NoSingleElement
  | ValueConflict (Set a)
  | NoMcdcCoverage (ValueAssignment a)
  deriving (Show)

-- | Join 2 ValueAssignments using 'and' logic: Take all TruthVectors from
-- @a and merge them with a true value from @b (because True is the identity of 'and')
-- Take all TruthVectors from @b and merge them with a true value from @a.
-- Reassemble the items into their new true and false value sets and return the new
-- TruthVector.
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

joinAnd2 ::
     (Ord a)
  => ValueAssignment a
  -> ValueAssignment a
  -> Either (JoinError a) (ValueAssignment a)
joinAnd2 a b = if hasMcdcCoverage tv then Right tv else Left (NoMcdcCoverage tv)
 where
  tv = TruthVector (S.union tf ft) tt
  tt = joinVals2 (trues a) (trues b)
  tf = joinVals2 (trues a) (falses b)
  ft = joinVals2 (falses a) (trues b)
-- | Same as joinAnd, but for or. Implemented using the equivalence "A || B == !(!A && !B)"
joinOr ::
     (Ord a)
  => ValueAssignment a
  -> ValueAssignment a
  -> Either (JoinError a) (ValueAssignment a)
joinOr a b = invert <$> joinAnd (invert a) (invert b)
joinOr2 ::
     (Ord a)
  => ValueAssignment a
  -> ValueAssignment a
  -> Either (JoinError a) (ValueAssignment a)
joinOr2 a b = invert <$> joinAnd2 (invert a) (invert b)

-- | BoolExp evaluator which gives a MCDC minimal set for a given expression.
findMcdc :: Ord a => BoolExp a -> Either (JoinError a) (ValueAssignment a)
findMcdc =
  monadEval (Evaluator (pure . invert) joinAnd2 joinOr2 (Right . singleton))
