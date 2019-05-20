module TruthVectors where

import           Data.Map        (Map)
import qualified Data.Map.Strict as M
import           Data.Set        (Set)
import qualified Data.Set        as S

type TruthVector a = Map a Bool

type TruthVectorSet a = Set (TruthVector a)

data ValueAssignment a = Val
  { falses :: TruthVectorSet a
  , trues  :: TruthVectorSet a
  } deriving (Show)

safeUnion :: (Ord a, Eq b) => Map a b -> Map a b -> Either (Set a) (Map a b)
safeUnion ma mb
  | null differentValues = Right $ M.union ma mb
  | otherwise = Left differentValues
  where
    commonKeys = S.intersection (M.keysSet ma) (M.keysSet mb)
    differentValues = S.filter (\k -> ma M.! k /= mb M.! k) commonKeys

joinVals ::
     Ord a
  => Set (Map a Bool)
  -> Map a Bool
  -> Either (Set a) (Set (Map a Bool))
joinVals as bs = S.fromList <$> sequence (safeUnion bs <$> S.toList as)

singleton :: (Ord a) => a -> ValueAssignment a
singleton a =
  Val
    (S.singleton (M.fromList [(a, False)]))
    (S.singleton (M.fromList [(a, True)]))

allVarsInAssignment :: (Ord a) => ValueAssignment a -> [a]
allVarsInAssignment ass =
  S.toList . S.unions . S.map M.keysSet $ S.union (falses ass) (trues ass)
