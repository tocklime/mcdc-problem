module McdcCheck where

import           Data.Maybe      (maybeToList)
import qualified Data.Set        as S
import           TruthVectors
import           ValueAssignment

-- | small MCDC check -- does the given 'ValueAssignment' have an entry in 'trues'
-- containing 'target', and an entry in 'falses' containing 'target', such that the
-- vectors only differ in the value assigned to 'target'?
hasMcdcPairFor :: Ord a => ValueAssignment a -> a -> Bool
hasMcdcPairFor ass target = not (null goodSets)
  where
    goodSets =
      [ (t, f)
      | t <- S.toList $ trues ass
      , f <- S.toList $ falses ass
      , tval <- maybeToList $ TruthVectors.lookup target t
      , fval <- maybeToList $ TruthVectors.lookup target f
      , tval /= fval
      , delete target t == delete target f
      ]

-- | large MCDC check -- does the 'ValueAssignment' have good pairs for all variables mentioned?
hasMcdcCoverage :: Ord a => ValueAssignment a -> Bool
hasMcdcCoverage ass = all (hasMcdcPairFor ass) (allVarsInAssignment ass)

-- | Is the size of the 'ValueAssignment' equal to one more than the number of variables mentioned?
isNPlus1Coverage :: (Ord a) => ValueAssignment a -> Bool
isNPlus1Coverage ass = varCount + 1 == caseCount
  where
    varCount = length (allVarsInAssignment ass)
    caseCount = S.size (trues ass) + S.size (falses ass)

-- | Is the 'ValueAssignment' correct and minimal?
isMinimalMcdcCoverage :: Ord a => ValueAssignment a -> Bool
isMinimalMcdcCoverage ass = isNPlus1Coverage ass && hasMcdcCoverage ass
