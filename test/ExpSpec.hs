{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module ExpSpec (spec) where

import Import
import Test.Hspec
import Test.Hspec.QuickCheck
import Evaluator
import qualified Data.Set as S

spec :: Spec
spec = do
  describe "expressions" $ do
    it "Literals return their value" $ eval (const True) (Lit 'a') `shouldBe` True
    it "not inverts" $ eval (const False) (Not (Lit 'a')) `shouldBe` True
    it "and works" $ eval (== 'a') (And (Lit 'a') (Lit 'b')) `shouldBe` False
    it "and works" $ eval (== 'a') (And (Lit 'a') (Lit 'a')) `shouldBe` True
    it "or works" $ eval (== 'a') (Or (Lit 'a') (Lit 'b')) `shouldBe` True
    it "or works" $ eval (== 'a') (Or (Lit 'b') (Lit 'b')) `shouldBe` False
    prop "orworks2" $ \(a::Int) (b::Int) -> eval (== a) (Or (Lit a) (Lit b)) `shouldBe` True
  describe "getVarList" $ do
    it "gets the list of variables" $ allVars (And (Lit 'a') (Or (Lit 'b') (Not (Lit 'c')))) `shouldBe` S.fromList "abc"
  describe "finding spanning set" $ do
    it "returns a single single choice" $ findSpanningSets @Int [[1]] `shouldBe` [[1]]
    it "returns all elements which don't overlap" $ findSpanningSets @Int [[1],[2]] `shouldBe` [[1,2]]
    it "returns all possible choices for single element" $ findSpanningSets @Int [[1,2]] `shouldBe` [[1],[2]]
    it "folds elements down when they appear" $ findSpanningSets @Int [[1,2],[1,2]] `shouldBe` [[1],[1,2],[2]]
    it "folds elements down when they appear2" $ findSpanningSets @Int [[1],[1,2]] `shouldBe` [[1],[1,2]]