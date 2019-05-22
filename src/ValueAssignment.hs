module ValueAssignment where

import qualified Data.Set     as S
import           TruthVectors

-- | A 'ValueAssignment' is a set of 'TruthVector's,
-- but separated out into those marked 'true' and 'false'. That is, it is a
-- 'TruthVector' of 'TruthVector's.
type ValueAssignment a = TruthVector (TruthVector a)

-- | Returns a vector where the single value 'a' is marked false in the false set, and true in the true set.
singleton :: Ord a => a -> ValueAssignment a
singleton a =
  TruthVector (S.singleton (singletonFalse a)) (S.singleton (singletonTrue a))

-- | Get all variables defined in all 'TruthVector's.
allVarsInAssignment :: (Ord a) => ValueAssignment a -> [a]
allVarsInAssignment ass = S.toList . S.unions . S.map elements $ elements ass
