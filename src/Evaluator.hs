{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Evaluator where

import Import
import Data.Set(Set)
import qualified Data.Set as S
import Data.List(intercalate,sort)
import qualified Data.Map.Strict as M

data Evaluator a b = Evaluator
    { evalDoNot :: b -> b
    , evalDoAnd :: b -> b -> b
    , evalDoOr :: b -> b -> b
    , evalLookup :: a -> b
    }

generalEval :: Evaluator a b -> BoolExp a -> b
generalEval e = go
  where
    go (Lit a) = ((evalLookup e) a)
    go (Not a) = (evalDoNot e) (go a)
    go (And a b) = (evalDoAnd e) (go a) (go b)
    go (Or a b) = (evalDoOr e) (go a) (go b)

allVars :: Ord a => BoolExp a -> Set a
allVars = generalEval (Evaluator id S.union S.union S.singleton)

eval :: (a -> Bool) -> BoolExp a -> Bool
eval = generalEval . (Evaluator not (&&) (||))

evalSet :: Ord a => (Set a) -> BoolExp a -> Bool
evalSet s = generalEval (Evaluator not (&&) (||) (\i -> S.member i s)) 

type TruthTable a = [(Set a, Bool)]

makeTruthTable :: (Ord a) => BoolExp a -> TruthTable a
makeTruthTable expr = (\s -> (s,evalSet s expr)) <$> allAssignments
    where
        vars = allVars expr
        allAssignments = S.toList $ S.powerSet vars
        
drawTruthTable :: (Ord a,Show a) => TruthTable a -> String
drawTruthTable tt = unlines $ header : line : (sort items)
  where
    header = (intercalate " " allTitles ) ++ " Result"
    line = replicate (length header) '-'
    items = (\(s,r) -> intercalate " " ( (showItem s <$> allItems) ++ [show r])) <$> tt
    showItem s n = (if n `S.member` s then 'T' else 'F') : (replicate ((-1+) . length . show $ n) ' ')
    allItems = S.toList $ S.unions $ fst <$> tt
    allTitles = show <$> allItems


allMcdcOptionsForTarget :: Ord a => BoolExp a -> a -> [Set a]
allMcdcOptionsForTarget expr target = do
    let vars = S.delete target $ allVars expr
    [v | v <- S.toList $ S.powerSet vars
       , evalSet v expr /= evalSet (S.insert target v) expr]

data ValueAssignment a = Val 
  { falses :: Set (Map a Bool)
  , trues :: Set (Map a Bool)
  } deriving Show

singletonValAss :: (Ord a) => a -> ValueAssignment a
singletonValAss a = Val (S.singleton (M.fromList [(a,False)]))  
                        (S.singleton (M.fromList [(a,True)]))

joinVals :: Ord a => Set (Map a Bool) -> Set (Map a Bool) -> Set (Map a Bool)
joinVals as bs = S.fromList (M.union <$> S.toList as <*> S.toList bs)

joinAnd :: Ord a => ValueAssignment a -> ValueAssignment a -> ValueAssignment a
joinAnd a b = Val fls trs
  where
    aTrueA = S.singleton $ S.findMin (trues a)
    aTrueB = S.singleton $ S.findMin (trues b)
    falseAsWithTrueB = joinVals (falses a) aTrueB
    trueAsWithTrueB = joinVals (trues a) aTrueB
    falseBsWithTrueA = joinVals (falses b) aTrueA
    trueBsWithTrueA = joinVals (trues b) aTrueA
    trs = S.union trueAsWithTrueB trueBsWithTrueA
    fls = S.union falseBsWithTrueA falseAsWithTrueB
  
joinOr :: Ord a => ValueAssignment a -> ValueAssignment a -> ValueAssignment a
joinOr a b = Val fls trs
  where
    aFalseA = S.singleton $ S.findMin (falses a)
    aFalseB = S.singleton $ S.findMin (falses b)
    trueAsWithFalseB = joinVals (trues a) aFalseB
    falseAsWithFalseB = joinVals (falses a) aFalseB
    trueBsWithFalseA = joinVals (trues b) aFalseA
    falseBsWithFalseA = joinVals (falses b) aFalseA
    trs = S.union trueAsWithFalseB trueBsWithFalseA
    fls = S.union falseAsWithFalseB falseBsWithFalseA
  
joinNot :: ValueAssignment a -> ValueAssignment a
joinNot (Val fs ts) = Val ts fs

findMcdc :: Ord a => BoolExp a -> ValueAssignment a
findMcdc = generalEval (Evaluator joinNot joinAnd joinOr singletonValAss)