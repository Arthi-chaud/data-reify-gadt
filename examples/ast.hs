{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Control.Monad
import Data.Reify.GADT
import Text.Printf

newtype Fix f a = Fx (f (Fix f) a)

-- Inspired by https://github.com/ku-fpg/data-reify/blob/master/examples/simplify.hs

data TreeF e a where
    Leaf :: (Show a) => a -> TreeF e a
    Node :: e a -> e a -> TreeF e a

type Tree = Fix TreeF

instance MuRef (Fix TreeF) where
    type DeRef (Fix TreeF) = TreeF
    type E (Fix TreeF) = Fix TreeF
    mapDeRef f (Fx e) = case e of
        Leaf a -> pure $ Leaf a
        Node l r -> Node <$> f l <*> f r

instance Show (TreeF Terminal f) where
    show e = case e of
        Leaf a -> "Leaf " ++ show a
        Node l r -> printf "Node (%s) (%s)" (show l) (show r)

-- Smart constructors

node :: Tree a -> Tree a -> Tree a
node l r = Fx $ Node l r

leaf :: (Show a) => a -> Tree a
leaf = Fx . Leaf

------

tree1 :: Tree Int
tree1 = node (node (leaf 1) tree1) tree2

tree2 :: Tree Int
tree2 = node tree1 (leaf 2)

main :: IO ()
main = do
    (Graph nodes root) <- reifyGraph tree1
    printf "Root: %d\n" root
    forM_ nodes $ \(u, MkNode n) -> do
        printf "%d: %s\n" u (show n)
