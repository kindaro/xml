-- | Generalized cursors to be applied to different nodes.
module Text.XML.Cursor.Generic
    ( -- * Core
      Cursor
    , Axis
    , toCursor
    , node
      -- * Axes
    , child
    , parent
    , precedingSibling
    , followingSibling
    , ancestor
    , descendant
    , orSelf
    , preceding
    , following
      -- * Operators
    , (&|)
    , (&/)
    , (&//)
    , (&.//)
    , ($|)
    , ($/)
    , ($//)
    , ($.//)
    , (>=>)
    ) where

import Data.Maybe (maybeToList)
import Data.List (foldl')
import Control.Monad ((>=>))

type DiffCursor node = [RecursiveCursor node] -> [RecursiveCursor node]
type Axis node = Cursor node -> [Cursor node]

data RecursiveCursor node = RecursiveCursor
    { _parent' :: Maybe (RecursiveCursor node)
    , _precedingSibling' :: DiffCursor node
    , _followingSibling' :: DiffCursor node
    -- | The child axis. XPath:
    -- /the child axis contains the children of the context node/.
    , _child' :: [RecursiveCursor node]
    -- | The current node.
    , _node' :: node
    }

-- | A cursor: contains an XML 'Node' and pointers to its children, ancestors and siblings.
data Cursor node = Cursor
    { _parent :: Maybe (Cursor node)
    , _preceding :: [Cursor node]
    , _following :: [Cursor node]
    , _children :: [Cursor node]
    , _node :: node
    } deriving (Eq, Ord)

eval :: RecursiveCursor node -> Cursor node
eval cursor = Cursor
    { _parent = eval <$> _parent' cursor
    , _preceding = eval <$> _precedingSibling' cursor []
    , _following = eval <$> _followingSibling' cursor []
    , _children = eval <$> _child' cursor
    , _node = _node' cursor
    }

child :: Axis node
child = _children

node :: Cursor node -> node
node = _node

instance Show node => Show (RecursiveCursor node) where
    show RecursiveCursor { _node' = n } = "RecursiveCursor @ " ++ show n

instance Show node => Show (Cursor node) where
    show Cursor { _node = n } = "Cursor @ " ++ show n

-- | Cursor smart constructor. This is where we launch the more convoluted
-- Recursive Cursor smart constructor from.
toCursor :: (node -> [node]) -> node -> Cursor node
toCursor getChildren = eval . toCursor' getChildren Nothing id id

-- | Recursive Cursor smart constructor. This is where we are building the expressions for
-- predecessors and followers.
toCursor' :: (node -> [node])
          -> Maybe (RecursiveCursor node)
          -> DiffCursor node
          -> DiffCursor node
          -> node
          -> RecursiveCursor node
toCursor' cs par pre fol n =
    me
  where
    me = RecursiveCursor par pre fol chi n
    chi' = cs n
    chi = go id chi' []
    go _ [] = id
    go pre' (n':ns') =
        (:) me' . fol'
      where
        me' = toCursor' cs (Just me) pre' fol' n'
        fol' = go (pre' . (:) me') ns'

-- | The parent axis. As described in XPath:
-- /the parent axis contains the parent of the context node, if there is one/.
--
-- Every node but the root element of the document has a parent. Parent nodes
-- will always be 'NodeElement's.
parent :: Axis node
parent = maybeToList . _parent

-- | The preceding-sibling axis. XPath:
-- /the preceding-sibling axis contains all the preceding siblings of the context node [...]/.
precedingSibling :: Axis node
precedingSibling = _preceding

-- | The following-sibling axis. XPath:
-- /the following-sibling axis contains all the following siblings of the context node [...]/.
followingSibling :: Axis node
followingSibling = _following

-- | The preceding axis. XPath:
-- /the preceding axis contains all nodes in the same document as the context node that are before the context node in document order, excluding any ancestors and excluding attribute nodes and namespace nodes/.
preceding :: Axis node
preceding c =
    go (_preceding c) (parent c >>= preceding)
  where
    go x y = foldl' (flip go') y x
    go' x rest = foldl' (flip  go') (x : rest) (child x)

-- | The following axis. XPath:
-- /the following axis contains all nodes in the same document as the context node that are after the context node in document order, excluding any descendants and excluding attribute nodes and namespace nodes/.
following :: Axis node
following c =
    go (_following c) (parent c >>= following)
  where
    go x z = foldr go' z x
    go' x rest = x : foldr go' rest (child x)

-- | The ancestor axis. XPath:
-- /the ancestor axis contains the ancestors of the context node; the ancestors of the context node consist of the parent of context node and the parent's parent and so on; thus, the ancestor axis will always include the root node, unless the context node is the root node/.
ancestor :: Axis node
ancestor = parent >=> (\p -> p : ancestor p)

-- | The descendant axis. XPath:
-- /the descendant axis contains the descendants of the context node; a descendant is a child or a child of a child and so on; thus the descendant axis never contains attribute or namespace nodes/.
descendant :: Axis node
descendant = child >=> (\c -> c : descendant c)

-- | Modify an axis by adding the context node itself as the first element of the result list.
orSelf :: Axis node -> Axis node
orSelf ax c = c : ax c

infixr 1 &|
infixr 1 &/ 
infixr 1 &// 
infixr 1 &.// 
infixr 1 $|
infixr 1 $/
infixr 1 $//
infixr 1 $.//

-- | Apply a function to the result of an axis.
(&|) :: (Cursor node -> [a]) -> (a -> b) -> (Cursor node -> [b])
f &| g = map g . f

-- | Combine two axes so that the second works on the children of the results
-- of the first.
(&/) :: Axis node -> (Cursor node -> [a]) -> (Cursor node -> [a])
f &/ g = f >=> child >=> g

-- | Combine two axes so that the second works on the descendants of the results
-- of the first.
(&//) :: Axis node -> (Cursor node -> [a]) -> (Cursor node -> [a])
f &// g = f >=> descendant >=> g

-- | Combine two axes so that the second works on both the result nodes, and their
-- descendants.
(&.//) :: Axis node -> (Cursor node -> [a]) -> (Cursor node -> [a])
f &.// g = f >=> orSelf descendant >=> g

-- | Apply an axis to a 'Cursor node'.
($|) :: Cursor node -> (Cursor node -> a) -> a
v $| f = f v

-- | Apply an axis to the children of a 'Cursor node'.
($/) :: Cursor node -> (Cursor node -> [a]) -> [a]
v $/ f = child v >>= f

-- | Apply an axis to the descendants of a 'Cursor node'.
($//) :: Cursor node -> (Cursor node -> [a]) -> [a]
v $// f = descendant v >>= f

-- | Apply an axis to a 'Cursor node' as well as its descendants.
($.//) :: Cursor node -> (Cursor node -> [a]) -> [a]
v $.// f = orSelf descendant v >>= f
