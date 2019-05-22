module NaiveMcdc where

import qualified Data.Set        as S
import           Expressions
import           McdcCheck
import           TruthVectors
import           ValueAssignment

-- | Super naive implementation to find all mcdc -- Generates the full truth
-- table (powerset of variables), and takes the power set of that to get all
-- possible sets of truth table lines, then filters them for MCDC minimalness.
-- Performance of this is obviously terrible, but it's useful for checking smarter
-- implementations with small inputs.
findAllMcdcNaive :: Ord a => BoolExp a -> S.Set (ValueAssignment a)
findAllMcdcNaive a = S.filter isMinimalMcdcCoverage allPossibleValueAssignments
  where
    vars = allVars a
    allSetsOfVars = S.powerSet vars
    allPossibleTruthVectors = S.map mkValAss allSetsOfVars
    allPossibleTruthVectorSets = S.powerSet allPossibleTruthVectors
    allPossibleValueAssignments =
      S.map
        (fromFuncAndSet (\tv -> evalSet (trues tv) a))
        allPossibleTruthVectorSets
    mkValAss ts = TruthVector (S.difference vars ts) ts
