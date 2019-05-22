{-# LANGUAGE TupleSections #-}

module TruthVectors where

import           Data.Set (Set)
import qualified Data.Set as S

-- | A 'TruthVector' of a is a pair of sets 'falses' and 'trues'. An element is false if it is in
-- the first set, true if it is in the second. If it is in neither the value is not (yet) defined
-- and if in both then the vector has a conflict.
data TruthVector a = TruthVector
  { falses, trues :: Set a
  } deriving (Show, Ord, Eq)

-- | The empty 'TruthVector'
empty :: TruthVector a
empty = TruthVector S.empty S.empty

-- | Insert a new variable with the given value.
insert :: Ord a => a -> Bool -> TruthVector a -> TruthVector a
insert a isTrue (TruthVector fs ts)
  | isTrue = TruthVector fs (S.insert a ts)
  | otherwise = TruthVector (S.insert a fs) ts

-- | Take a single value and mark it true in a new 'TruthVector'
singletonTrue :: Ord a => a -> TruthVector a
singletonTrue a = insert a True empty

-- | Take a single value and mark it false in a new 'TruthVector'
singletonFalse :: Ord a => a -> TruthVector a
singletonFalse a = insert a False empty

-- | Create a 'TruthVector' from a list of false items and a list of true items.
fromLists :: Ord a => [a] -> [a] -> TruthVector a
fromLists fs ts = TruthVector (S.fromList fs) (S.fromList ts)

-- | Create a 'TruthVector' from a list of items and a function to assign truth to them.
fromFuncAndSet :: Ord a => (a -> Bool) -> Set a -> TruthVector a
fromFuncAndSet f = foldr (\a -> insert a (f a)) empty

-- | Swap the falses and trues sets, making everything that was false true and vice versa.
invert :: TruthVector a -> TruthVector a
invert (TruthVector fs ts) = TruthVector ts fs

-- | Return a set of all elements defined in the 'TruthVector'
elements :: Ord a => TruthVector a -> Set a
elements (TruthVector fs ts) = S.union fs ts

-- | Unmark an item as true and false -- remove it from both sets.
delete :: Ord a => a -> TruthVector a -> TruthVector a
delete a tv = TruthVector (S.delete a (falses tv)) (S.delete a (trues tv))

-- | Determine the value of a given element. Returns Nothing for conflicted or undefined.
lookup :: Ord a => a -> TruthVector a -> Maybe Bool
lookup a tv =
  case (isTrue, isFalse) of
    (True, False) -> Just True
    (False, True) -> Just False
    _             -> Nothing
  where
    isTrue = a `S.member` trues tv
    isFalse = a `S.member` falses tv

-- | Merges 2 'TruthVector's naively. The resulting vector may have conflicts
merge :: Ord a => TruthVector a -> TruthVector a -> TruthVector a
merge a b =
  TruthVector (S.union (falses a) (falses b)) (S.union (trues a) (trues b))

-- | Find all contradictions in the 'TruthVector'.
contradictions :: Ord a => TruthVector a -> Set a
contradictions a = S.intersection (falses a) (trues a)

-- | Try to merge 2 'TruthVector's. If the resulting 'TruthVector' is conflicted
-- then return 'Nothing', otherwise return the result.
safeMerge ::
     Ord a => TruthVector a -> TruthVector a -> Either (Set a) (TruthVector a)
safeMerge a b
  | S.null contras = Right merged
  | otherwise = Left contras
  where
    merged = merge a b
    contras = contradictions merged

-- | Join each element of the set 'as' with 'b'. If any of the resulting TruthVectors are
-- conflicted then return a 'Left' value containing the first conflicts found. Otherwise return
-- the new 'TruthVector's.
joinVals ::
     Ord a
  => Set (TruthVector a)
  -> TruthVector a
  -> Either (Set a) (Set (TruthVector a))
joinVals as b = S.fromList <$> sequence (safeMerge b <$> S.toList as)

-- | Join each element of the set 'as' with each element of the set 'bs'. If any of the resulting TruthVectors are
-- conflicted then return a 'Left' value containing the first conflicts found. Otherwise return
-- the new 'TruthVector's.
joinVals2 ::
     Ord a => Set (TruthVector a) -> Set (TruthVector a) -> Set (TruthVector a)
joinVals2 as bs =
  S.fromList
    [ c
    | a <- S.toList as
    , b <- S.toList bs
    , let c = merge a b
    , S.null $ contradictions c
    ]

-- | Function to help print out a 'TruthVector' - given a false-printing function
-- and a true printing function and a 'TruthVector', produce a 'String'. Variables are output in order
-- Example:
-- @prettyPrint (:[]) ((:[]) . Char.toUpper) tv == "aBc"@
prettyPrint ::
     Ord a => (a -> String) -> (a -> String) -> TruthVector a -> String
prettyPrint ff tf tv = concat (showItem <$> S.toList allItems)
  where
    allItems = S.union (S.map (, False) (falses tv)) (S.map (, True) (trues tv))
    showItem (a, True)  = tf a
    showItem (a, False) = ff a

-- | Function to help print out a 'TruthVector' -- given an element-printing function,
-- first print all false elements, then all true elements.
-- Example:
-- @prettyPrint (:[]) ((:[]) . Char.toUpper) tv == "FALSE: ac TRUE: b"@
prettyPrint2 :: (a -> String) -> TruthVector a -> String
prettyPrint2 f tv =
  "FALSE: " ++ render (falses tv) ++ " TRUE: " ++ render (trues tv)
  where
    render = unwords . fmap f . S.toList
