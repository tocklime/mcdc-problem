{-# LANGUAGE ScopedTypeVariables #-}

module ExpSpec
  ( spec
  ) where

import           ConstructiveMcdc
import           Data.Either           (isRight)
import qualified Data.Set              as S
import           Expressions
import           McdcCheck
--import           NaiveMcdc
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import qualified TruthVectors          as TV
import Parser(parseUnsafe)

newtype TestBE = TestBE
  { unTest :: BoolExp Char
  } deriving (Show)

instance Arbitrary TestBE where
  arbitrary = TestBE <$> sized exp'
    where
      exp' n
        | n <= 0 = Var <$> oneof (pure <$> ['a'..'z'])
        | otherwise =
          oneof
            [ Var <$> oneof (pure <$> ['a'..'z'])
            , Not <$> sub
            , Or <$> sub <*> sub
            , And <$> sub <*> sub
            ]
        where
          sub = exp' (n `div` 2)

spec :: Spec
spec =
  modifyMaxSuccess (const 100) $ do
    describe "expressions" $ do
      it "Literals return their value" $
        eval (const True) (p "a") `shouldBe` True
      it "not inverts" $ eval (const False) (Not (Var 'a')) `shouldBe` True
      it "and works" $ eval (== 'a') (p "a && b") `shouldBe` False
      it "and works" $ eval (== 'a') (p "a && a") `shouldBe` True
      it "or works" $ eval (== 'a') (p "a || b") `shouldBe` True
      it "or works" $ eval (== 'a') (p "b || b") `shouldBe` False
      prop "orworks2" $ \(a :: Int) (b :: Int) ->
        eval (== a) (Or (Var a) (Var b)) `shouldBe` True
    describe "getVarLst" $
      it "gets the list of variables" $
      allVars (p "a && (b || !c)") `shouldBe`
      S.fromList "abc"
    describe "safe unions" $ do
      it "merges distinct key sets" $
        TV.safeMerge (TV.fromLists "" "a") (TV.fromLists "" "b")  `shouldBe`
        Right (TV.fromLists "" "ab")
      it "merges like keys with like values sets" $
        TV.safeMerge (TV.fromLists "" "a") (TV.fromLists "" "a") `shouldBe`
        Right (TV.fromLists "" "a")
      it "fails when there are like keys with different values" $
        TV.safeMerge (TV.fromLists "a" "") (TV.fromLists "" "a") `shouldBe`
        Left (S.fromList ['a'])
    describe "MCDC generator" $ modifyMaxSize (const 5) $ do
      it "can solve A&A" $
        isRight . findMcdc $ (p "a && a")
        {- performance of this is horrid
      prop "finds a solution if the naive one does" $ \(TestBE e) ->
        let constr = findMcdc e
            naive = findAllMcdcNaive e
        in isRight constr || null naive
        -}
      prop "the test cases generated return the given values" $ \(TestBE e) ->
        case findMcdc e of
          Left _ -> property Discard
          Right a ->
            property $
            all (\tv -> evalSet (TV.trues tv) e) (S.toList (TV.trues a)) &&
            all (\tv -> not $ evalSet (TV.trues tv) e) (S.toList (TV.falses a))
      prop "always generates minimal coverage test cases" $ \(TestBE e) ->
        case findMcdc e of
          Left _  -> property Discard
          Right a -> property $ isMinimalMcdcCoverage a
  where
    p = parseUnsafe
