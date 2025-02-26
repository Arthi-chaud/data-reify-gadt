{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Reify.GADT (
    MuRef (..),
    reifyGraph,
    reifyGraphs,
    module Data.Reify.GADT.Graph,
) where

import Control.Concurrent
import Data.HashMap.Lazy
import qualified Data.HashMap.Lazy as HM
import Data.Hashable
import Data.IntSet
import qualified Data.IntSet as IS
import Data.Kind
import Data.Reify.GADT.Graph
import System.Mem.StableName

-- | 'MuRef' is a class that provided a way to reference into a specific type,
-- and a way to map over the deferenced internals.
class MuRef (a :: Type -> Type) where
    type DeRef a :: (Type -> Type) -> (Type -> Type)
    type E a :: Type -> Type
    mapDeRef ::
        (Applicative f, e ~ E a) =>
        (forall t'. (MuRef e) => e t' -> f (u t')) ->
        a t ->
        f ((DeRef a) u t)

-- | 'reifyGraph' takes a data structure that admits 'MuRef', and returns a 'Graph' that contains
-- the dereferenced nodes, with their children as 'Unique's rather than recursive values.
reifyGraph ::
    (MuRef s, E (E s) ~ E s, DeRef (E s) ~ DeRef s) =>
    s a ->
    IO (Graph (DeRef s) a)
reifyGraph m = do
    rt1 <- newMVar HM.empty
    uVar <- newMVar 0
    reifyWithContext rt1 uVar m

-- | 'reifyGraphs' takes a 'Traversable' container 't s' of a data structure 's'
-- admitting 'MuRef', and returns a 't (Graph (DeRef s))' with the graph nodes
-- resolved within the same context.
--
-- This allows for, e.g., a list of mutually recursive structures.
reifyGraphs ::
    (MuRef s, E (E s) ~ E s, DeRef (E s) ~ DeRef s, Traversable t) =>
    t (s e) ->
    IO (t (Graph (DeRef s) e))
reifyGraphs coll = do
    rt1 <- newMVar HM.empty
    uVar <- newMVar 0
    traverse (reifyWithContext rt1 uVar) coll

-- NB: We deliberately reuse the same map of stable
-- names and unique supply across all iterations of the
-- traversal to ensure that the same context is used
-- when reifying all elements of the container.

-- Reify a data structure's 'Graph' using the supplied map of stable names and
-- unique supply.
reifyWithContext ::
    (MuRef s, E (E s) ~ E s, DeRef (E s) ~ DeRef s) =>
    MVar (HashMap DynStableName Unique) ->
    MVar Unique ->
    s a ->
    IO (Graph (DeRef s) a)
reifyWithContext rt1 uVar j = do
    rt2 <- newMVar []
    nodeSetVar <- newMVar IS.empty
    Terminal root <- findNodes rt1 rt2 uVar nodeSetVar j
    pairs <- readMVar rt2
    return (Graph pairs root)

-- The workhorse for 'reifyGraph' and 'reifyGraphs'.
findNodes ::
    -- ‘DeRef (E (e t) t') ~ DeRef (e t')’
    (MuRef s, E (E s) ~ E s, DeRef (E s) ~ DeRef s) =>
    -- | A map of stable names to unique numbers.
    --   Invariant: all 'Uniques' that appear in the range are less
    --   than the current value in the unique name supply.
    MVar (HashMap DynStableName Unique) ->
    -- | The key-value pairs in the 'Graph' that is being built.
    --   Invariant 1: the domain of this association list is a subset
    --   of the range of the map of stable names.
    --   Invariant 2: the domain of this association list will never
    --   contain duplicate keys.
    -- MVar [(Unique,DeRef s Unique)]
    MVar [(Unique, Node (DeRef s))] ->
    -- | A supply of unique names.
    MVar Unique ->
    -- | The unique numbers that we have encountered so far.
    --   Invariant: this set is a subset of the range of the map of
    --   stable names.
    MVar IntSet ->
    -- | The value for which we will reify a 'Graph'.
    s t ->
    -- | The unique number for the value above.
    IO (Terminal t)
findNodes rt1 rt2 uVar nodeSetVar !j = do
    st <- makeDynStableName j
    tab <- takeMVar rt1
    nodeSet <- takeMVar nodeSetVar
    case HM.lookup st tab of
        Just var -> do
            putMVar rt1 tab
            if var `IS.member` nodeSet
                then do
                    putMVar nodeSetVar nodeSet
                    return $ Terminal var
                else recurse var nodeSet
        Nothing -> do
            var <- newUnique uVar
            putMVar rt1 $ HM.insert st var tab
            recurse var nodeSet
  where
    recurse :: Unique -> IntSet -> IO (Terminal a)
    recurse var nodeSet = do
        putMVar nodeSetVar $ IS.insert var nodeSet
        res <- mapDeRef (findNodes rt1 rt2 uVar nodeSetVar) j
        tab' <- takeMVar rt2
        putMVar rt2 $ (var, MkNode res) : tab'
        return $ Terminal var

newUnique :: MVar Unique -> IO Unique
newUnique var = do
    v <- takeMVar var
    let v' = succ v
    putMVar var v'
    return v'

-- Stable names that do not use phantom types.
-- As suggested by Ganesh Sittampalam.
-- Note: GHC can't unpack these because of the existential
-- quantification, but there doesn't seem to be much
-- potential to unpack them anyway.
data DynStableName = forall a. DynStableName !(StableName a)

instance Hashable DynStableName where
    hashWithSalt s (DynStableName n) = hashWithSalt s n

instance Eq DynStableName where
    DynStableName m == DynStableName n =
        eqStableName m n

makeDynStableName :: a -> IO DynStableName
makeDynStableName a = do
    st <- makeStableName a
    return $ DynStableName st
