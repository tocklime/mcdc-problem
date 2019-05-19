{-# LANGUAGE RecordWildCards #-}
module Evaluator where

import Data.Set(Set)
import Control.Monad.Identity
import qualified Data.Set as S
import Data.Map(Map)
import qualified Data.Map.Strict as M

data BoolExp a = Lit a
               | And (BoolExp a) (BoolExp a)
               | Or (BoolExp a) (BoolExp a)
               | Not (BoolExp a)
               deriving (Eq, Show)


data Evaluator a b m = Evaluator
    { evalDoNot :: b -> m b
    , evalDoAnd :: b -> b -> m b
    , evalDoOr :: b -> b -> m b
    , evalLookup :: a -> m b
    }

data ValueAssignment a = Val 
  { falses :: Set (Map a Bool)
  , trues :: Set (Map a Bool)
  } deriving Show

monadEval :: Monad m => Evaluator a b m -> BoolExp a -> m b
monadEval Evaluator{..} = go
  where
    go (Lit a) = evalLookup a
    go (Not a) = go a >>= evalDoNot
    go (And a b) = go a >>= \ga -> go b >>= evalDoAnd ga
    go (Or a b) = go a >>= \ga -> go b >>= evalDoOr ga

generalEval :: Evaluator a b Identity -> BoolExp a -> b
generalEval e expr = runIdentity (monadEval e expr)

pure2 :: Monad m => (a -> b -> c) -> (a -> b -> m c)
pure2 f = (pure .). f

allVars :: Ord a => BoolExp a -> Set a
allVars = generalEval (Evaluator pure (pure2 S.union) (pure2 S.union) (pure . S.singleton))

eval :: (a -> Bool) -> BoolExp a -> Bool
eval f = generalEval (Evaluator (pure . not) (pure2 (&&)) (pure2 (||)) (pure . f))

evalSet :: Ord a => Set a -> BoolExp a -> Bool
evalSet s = generalEval (Evaluator (pure . not) (pure2 (&&)) (pure2 (||)) (\i -> pure $ S.member i s)) 

singletonValAss :: (Ord a) => a -> ValueAssignment a
singletonValAss a = Val (S.singleton (M.fromList [(a,False)]))  
                        (S.singleton (M.fromList [(a,True)]))

safeUnion :: (Ord a,Eq b) => Map a b -> Map a b -> Maybe (Map a b)
safeUnion ma mb | allSame = Just $ M.union ma mb
                | otherwise = Nothing
  where
    aKeys = M.keysSet ma
    bKeys = M.keysSet mb
    commonKeys = S.intersection aKeys bKeys
    allSame = all (\k -> ma M.! k == mb M.! k) commonKeys
  
joinVals :: Ord a => Set (Map a Bool) -> Map a Bool -> Maybe (Set (Map a Bool))
joinVals as bs = S.fromList <$> sequence (safeUnion bs <$> S.toList as)

(<?>) :: Maybe a -> e -> Either e a
Just a  <?> _ = Right a
Nothing <?> e = Left e

data JoinMode = JoinAnd | JoinOr
data JoinError = NoSingleElement | ValueConflict deriving (Show)

joinWith :: (Ord a) => JoinMode -> ValueAssignment a -> ValueAssignment a -> Either JoinError (ValueAssignment a)
joinWith mode a b = do
    aA <- S.lookupMin (f1 a) <?> NoSingleElement
    aB <- S.lookupMin (f1 b) <?> NoSingleElement
    asWithB1 <- joinVals (f1 a) aB <?> ValueConflict
    asWithB2 <- joinVals (f2 a) aB <?> ValueConflict
    bsWithA1 <- joinVals (f1 b) aA <?> ValueConflict
    bsWithA2 <- joinVals (f2 b) aA <?> ValueConflict
    let xx = S.union asWithB1 bsWithA1
    let xy = S.union asWithB2 bsWithA2
    return $ pf Val xy xx
  where
    (f1,f2,pf) = case mode of
                JoinAnd -> (trues,falses,id)
                JoinOr -> (falses,trues,flip)
  
joinNot :: ValueAssignment a -> Either e (ValueAssignment a)
joinNot (Val fs ts) = pure $ Val ts fs

findMcdc :: Ord a => BoolExp a -> Either JoinError (ValueAssignment a)
findMcdc = monadEval (Evaluator joinNot (joinWith JoinAnd) (joinWith JoinOr) (Right . singletonValAss))

hasMcdcPairFor :: Ord a => ValueAssignment a -> a -> Bool
hasMcdcPairFor ass target = not (null goodSets)
  where
    goodSets = [(t,f)| t <- S.toList $ trues ass
                     , f <- S.toList $ falses ass
                     , target `M.member` t
                     , target `M.member` f
                     , t M.! target /= f M.! target
                     , M.delete target t == M.delete target f]

allVarsInAssignment :: (Ord a) => ValueAssignment a -> [a]
allVarsInAssignment ass = S.toList . S.unions . S.map M.keysSet $ S.union (falses ass) (trues ass)

hasMcdcCoverage :: Ord a => ValueAssignment a -> Bool
hasMcdcCoverage ass = all (hasMcdcPairFor ass) (allVarsInAssignment ass)
    
isNPlus1Coverage :: (Ord a) => ValueAssignment a -> Bool
isNPlus1Coverage ass = varCount + 1 == caseCount
  where
    varCount = length (allVarsInAssignment ass)
    caseCount = S.size (trues ass) + S.size (falses ass)

isMinimalMcdcCoverage :: Ord a => ValueAssignment a -> Bool
isMinimalMcdcCoverage ass = isNPlus1Coverage ass && hasMcdcCoverage ass