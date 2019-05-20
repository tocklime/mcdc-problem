module NaiveMcdc where

import qualified Data.Set        as S
import           Expressions
import           McdcCheck
import           TruthVectors
import           ValueAssignment

findAllMcdcNaive :: Ord a => BoolExp a -> S.Set (ValueAssignment a)
findAllMcdcNaive a = S.filter isMinimalMcdcCoverage $ allPossibleValueAssignments
  where
    vars = allVars a
    allSetsOfVars = S.powerSet vars
    allPossibleTruthVectors = S.map mkValAss allSetsOfVars
    allPossibleTruthVectorSets = S.powerSet allPossibleTruthVectors
    allPossibleValueAssignments = S.map (fromFuncAndSet (\tv -> evalSet (trues tv) a)) allPossibleTruthVectorSets
    mkValAss ts = TruthVector (S.difference vars ts) ts
