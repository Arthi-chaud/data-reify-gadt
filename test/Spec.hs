{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Reify.GADT
import Test.Hspec
import Unsafe.Coerce

data ExprF e a where
    Val :: a -> ExprF e a
    Dup :: e a -> ExprF e (a, a)
    Pair :: e a -> e b -> ExprF e (a, b)
    Fst :: e (a, b) -> ExprF e a
    Snd :: e (a, b) -> ExprF e b
    Max :: (Ord a) => e a -> e a -> ExprF e a

newtype Fix f a = Fx (f (Fix f) a)

instance MuRef (Fix ExprF) where
    type DeRef (Fix ExprF) = ExprF
    type E (Fix ExprF) = Fix ExprF
    mapDeRef f (Fx e) = case e of
        Val a -> pure $ Val a
        Dup a -> Dup <$> f a
        Pair a b -> Pair <$> f a <*> f b
        Fst a -> Fst <$> f a
        Snd a -> Snd <$> f a
        Max a b -> Max <$> f a <*> f b

ast :: Fix ExprF Int
ast = Fx $ Max (Fx (Val (2 :: Int))) (Fx $ Snd $ Fx $ Pair ast (Fx $ Val 1))

evalGraph :: Graph (DeRef (Fix ExprF)) a -> a
evalGraph g@(Graph _ root) = evalNode g $ Terminal root

evalNode :: Graph (DeRef (Fix ExprF)) b -> Terminal a -> a
evalNode g@(Graph nodes _) (Terminal u) = case lookup u nodes of
    Nothing -> error "Node not found"
    Just (MkNode e) -> case unsafeCoerce e :: ExprF Terminal a of
        Val a -> a
        Dup a -> let res = evalNode g a in (res, res)
        Pair a b -> (evalNode g a, evalNode g b)
        Fst a -> fst $ evalNode g a
        Snd a -> snd $ evalNode g a
        Max a b -> max (evalNode g a) (evalNode g b)

specs :: Spec
specs = describe "Data.Reify.GADT" $ do
    describe "reifyGraph" $ do
        it "Compute and eval graph" $ do
            g@(Graph nodes root) <- reifyGraph ast
            root `shouldBe` 1
            length nodes `shouldBe` 5
            evalGraph g `shouldBe` 2

main :: IO ()
main = hspec specs
