module Evaluator where

import           Data.Either.Combinators (mapLeft)
import qualified Data.Map.Strict         as M
import           Data.Set                (Set)
import qualified Data.Set                as S
import           Expressions
import           TruthVectors

(<?>) :: Maybe a -> e -> Either e a
Just a <?> _ = Right a
Nothing <?> e = Left e

data JoinError a
  = NoSingleElement
  | ValueConflict (Set a)
  deriving (Show)

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
  return $ Val xy xx

joinOr ::
     (Ord a)
  => ValueAssignment a
  -> ValueAssignment a
  -> Either (JoinError a) (ValueAssignment a)
joinOr a b = joinNot <$> joinAnd (joinNot a) (joinNot b)

joinNot :: ValueAssignment a -> ValueAssignment a
joinNot = Val <$> trues <*> falses

findMcdc :: Ord a => BoolExp a -> Either (JoinError a) (ValueAssignment a)
findMcdc =
  monadEval (Evaluator (pure . joinNot) joinAnd joinOr (Right . singleton))

hasMcdcPairFor :: Ord a => ValueAssignment a -> a -> Bool
hasMcdcPairFor ass target = not (null goodSets)
  where
    goodSets =
      [ (t, f)
      | t <- S.toList $ trues ass
      , f <- S.toList $ falses ass
      , target `M.member` t
      , target `M.member` f
      , t M.! target /= f M.! target
      , M.delete target t == M.delete target f
      ]

hasMcdcCoverage :: Ord a => ValueAssignment a -> Bool
hasMcdcCoverage ass = all (hasMcdcPairFor ass) (allVarsInAssignment ass)

isNPlus1Coverage :: (Ord a) => ValueAssignment a -> Bool
isNPlus1Coverage ass = varCount + 1 == caseCount
  where
    varCount = length (allVarsInAssignment ass)
    caseCount = S.size (trues ass) + S.size (falses ass)

isMinimalMcdcCoverage :: Ord a => ValueAssignment a -> Bool
isMinimalMcdcCoverage ass = isNPlus1Coverage ass && hasMcdcCoverage ass
