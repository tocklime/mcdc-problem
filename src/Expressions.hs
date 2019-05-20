{-# LANGUAGE RecordWildCards #-}

module Expressions where

import           Control.Monad.Identity
import           Data.Set               (Set)
import qualified Data.Set               as S

data BoolExp a
  = Lit a
  | And (BoolExp a)
        (BoolExp a)
  | Or (BoolExp a)
       (BoolExp a)
  | Not (BoolExp a)
  deriving (Eq, Show)

data Evaluator a b m = Evaluator
  { evalDoNot  :: b -> m b
  , evalDoAnd  :: b -> b -> m b
  , evalDoOr   :: b -> b -> m b
  , evalLookup :: a -> m b
  }

pure2 :: Monad m => (a -> b -> c) -> (a -> b -> m c)
pure2 f = (pure .) . f

monadEval :: Monad m => Evaluator a b m -> BoolExp a -> m b
monadEval Evaluator {..} = go
  where
    go (Lit a)   = evalLookup a
    go (Not a)   = go a >>= evalDoNot
    go (And a b) = go a >>= \ga -> go b >>= evalDoAnd ga
    go (Or a b)  = go a >>= \ga -> go b >>= evalDoOr ga

generalEval :: Evaluator a b Identity -> BoolExp a -> b
generalEval e expr = runIdentity (monadEval e expr)

allVars :: Ord a => BoolExp a -> Set a
allVars =
  generalEval
    (Evaluator pure (pure2 S.union) (pure2 S.union) (pure . S.singleton))

eval :: (a -> Bool) -> BoolExp a -> Bool
eval f =
  generalEval (Evaluator (pure . not) (pure2 (&&)) (pure2 (||)) (pure . f))

evalSet :: Ord a => Set a -> BoolExp a -> Bool
evalSet s =
  generalEval
    (Evaluator
       (pure . not)
       (pure2 (&&))
       (pure2 (||))
       (\i -> pure $ S.member i s))
