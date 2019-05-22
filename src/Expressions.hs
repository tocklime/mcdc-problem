{-# LANGUAGE RecordWildCards #-}

module Expressions where

import           Control.Monad.Identity
import           Data.Set               (Set)
import qualified Data.Set               as S

-- | A basic boolean expression tree.
data BoolExp a
  = Lit a
  | And (BoolExp a)
        (BoolExp a)
  | Or (BoolExp a)
       (BoolExp a)
  | Not (BoolExp a)
  deriving (Eq, Show)

-- | Functions for how to deal with a 'BoolExp a', in some monad 'm'.
data Evaluator a b m = Evaluator
  { evalDoNot  :: b -> m b
  , evalDoAnd  :: b -> b -> m b
  , evalDoOr   :: b -> b -> m b
  , evalLookup :: a -> m b
  }

-- | Smart constructor which embeds the pure functions given in the 'Identity'
-- Monad to make an 'Evaluator'
pureEvaluator ::
     (b -> b)
  -> (b -> b -> b)
  -> (b -> b -> b)
  -> (a -> b)
  -> Evaluator a b Identity
pureEvaluator donot doand door dolookup =
  Evaluator
    (pure . donot)
    ((pure .) . doand)
    ((pure .) . door)
    (pure . dolookup)

-- | Evaluate an expression using the given evaluator in a Monad.
monadEval :: Monad m => Evaluator a b m -> BoolExp a -> m b
monadEval Evaluator {..} = go
  where
    go (Lit a)   = evalLookup a
    go (Not a)   = go a >>= evalDoNot
    go (And a b) = go a >>= \ga -> go b >>= evalDoAnd ga
    go (Or a b)  = go a >>= \ga -> go b >>= evalDoOr ga

-- | Evaluate and expression using the 'Identity' monad and unwrap.
generalEval :: Evaluator a b Identity -> BoolExp a -> b
generalEval e expr = runIdentity (monadEval e expr)

-- | find all variables in expression
allVars :: Ord a => BoolExp a -> Set a
allVars = generalEval (pureEvaluator id S.union S.union S.singleton)

-- | Evaluate the value of an expression using the given name to boolean value mapping.
eval :: (a -> Bool) -> BoolExp a -> Bool
eval f = generalEval (pureEvaluator not (&&) (||) f)

-- | Evaluate the value of an expression using the given set of names of true variables.
evalSet :: Ord a => Set a -> BoolExp a -> Bool
evalSet s = generalEval (pureEvaluator not (&&) (||) (`S.member` s))
