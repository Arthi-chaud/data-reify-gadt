module Data.Reify.GADT.Graph (
    Unique,
    Node (..),
    Graph (..),
    Terminal (..),
) where

-- 'Label' for a node in the graph
type Unique = Int

data Node e = forall t. MkNode (e Terminal t)

instance (forall t. Show (e Terminal t)) => Show (Node e) where
    show (MkNode e) = show e

data Graph e a = Graph [(Unique, Node e)] Unique

-- An AST node that refers to a Graph Node
newtype Terminal a = Terminal {unTerminal :: Unique}

instance Show (Terminal a) where
    show (Terminal a) = "Terminal " ++ show a
