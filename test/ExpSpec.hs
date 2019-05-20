{-# LANGUAGE ScopedTypeVariables #-}

module ExpSpec
  ( spec
  ) where

import qualified Data.Map.Strict       as M
import qualified Data.Set              as S
import           Evaluator
import           Expressions
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           TruthVectors

newtype TestBE = TestBE
  { unTest :: BoolExp Char
  } deriving (Show)

instance Arbitrary TestBE where
  arbitrary = TestBE <$> sized exp'
    where
      exp' n
        | n <= 0 = Lit <$> arbitrary
        | otherwise =
          oneof
            [ Lit <$> arbitrary
            , Not <$> sub
            , Or <$> sub <*> sub
            , And <$> sub <*> sub
            ]
        where
          sub = exp' (n `div` 2)

spec :: Spec
spec =
  modifyMaxSuccess (const 1000) $ do
    describe "expressions" $ do
      it "Literals return their value" $
        eval (const True) (Lit 'a') `shouldBe` True
      it "not inverts" $ eval (const False) (Not (Lit 'a')) `shouldBe` True
      it "and works" $ eval (== 'a') (And (Lit 'a') (Lit 'b')) `shouldBe` False
      it "and works" $ eval (== 'a') (And (Lit 'a') (Lit 'a')) `shouldBe` True
      it "or works" $ eval (== 'a') (Or (Lit 'a') (Lit 'b')) `shouldBe` True
      it "or works" $ eval (== 'a') (Or (Lit 'b') (Lit 'b')) `shouldBe` False
      prop "orworks2" $ \(a :: Int) (b :: Int) ->
        eval (== a) (Or (Lit a) (Lit b)) `shouldBe` True
    describe "getVarLst" $
      it "gets the list of variables" $
      allVars (And (Lit 'a') (Or (Lit 'b') (Not (Lit 'c')))) `shouldBe`
      S.fromList "abc"
    describe "safe unions" $ do
      it "merges distinct key sets" $
        safeUnion (M.fromList [('a', 'a')]) (M.fromList [('b', 'b')]) `shouldBe`
        Right (M.fromList [('a', 'a'), ('b', 'b')])
      it "merges like keys with like values sets" $
        safeUnion (M.fromList [('a', 'a')]) (M.fromList [('a', 'a')]) `shouldBe`
        Right (M.fromList [('a', 'a')])
      it "fails when there are like keys with different values" $
        safeUnion (M.fromList [('a', 'a')]) (M.fromList [('a', 'b')]) `shouldBe`
        Left (S.fromList ['a'])
    describe "MCDC generator" $ do
      prop "the test cases generated return the given values" $ \(TestBE e) ->
        case findMcdc e of
          Left _ -> property Discard
          Right a ->
            property $
            all (\m -> eval (\c -> m M.! c) e) (S.toList (trues a)) &&
            all (\m -> not $ eval (\c -> m M.! c) e) (S.toList (falses a))
      prop "always generates minimal coverage test cases" $ \(TestBE e) ->
        case findMcdc e of
          Left _  -> property Discard
          Right a -> property $ isMinimalMcdcCoverage a
