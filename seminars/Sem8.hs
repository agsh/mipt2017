module Sem8 where

data BinTree a
    = EmptyTree
    | Node a  (BinTree a) (BinTree a)
    deriving (Show, Ord)


singleton :: a -> BinTree a
singleton x = Node x EmptyTree EmptyTree


treeInsert :: Ord a => a -> BinTree a -> BinTree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node k left right)
    | x == k    = Node x left right
    | x < k     = Node k (treeInsert x left) right
    | otherwise = Node k left (treeInsert x right)


list2tree :: Ord a => [a] -> BinTree a
list2tree = foldl (flip treeInsert) EmptyTree


tree2list :: BinTree a -> [a]
tree2list EmptyTree         = []
tree2list (Node x left right) = tree2list left ++ [x] ++ tree2list right

treeSort :: Ord a => [a] -> [a]
treeSort = tree2list . list2tree

-- not a good way to make a long lambda

-- foldTree :: (t -> t1 -> t1 -> t1) -> t1 -> BinTree t -> t1
-- foldTree treeFunction listValue tree = loop tree id where
--     loop EmptyTree cont           = cont listValue
--     loop (Node x left right) cont = loop left (\leftAcc ->
--         loop right (\rightAcc ->
--             cont (treeFunction x leftAcc rightAcc)
--             )
--         )


foldBinTree :: (t -> t1 -> t1 -> t1) -> t1 -> BinTree t -> t1
foldBinTree treeFunction listValue = fold' where
    fold' EmptyTree           = listValue
    fold' (Node x left right) = treeFunction x (fold' left) (fold' right)


sumTree :: Num a => BinTree a -> a
sumTree = foldBinTree (\x a b -> x + a + b) 0

instance (Eq a) => Eq (BinTree a) where
    EmptyTree == EmptyTree                           = True
    EmptyTree == Node{}                              = False
    Node{} == EmptyTree                              = False
    (Node x1 left1 right1) == (Node x2 left2 right2) = x1 == x2 &&
                                                        left1 == left2 &&
                                                         right1 == right2


data Tree tLeaf tNode =
      LeafNode tLeaf
    | InternalNode tNode [Tree tLeaf tNode]


foldTree :: (t -> tleaf -> t) -> (t -> tnode -> t) -> t -> Tree tleaf tnode -> t
foldTree fLeaf fNode = fold' where
    fold' acc tree = case tree of
        LeafNode leafInfo              ->
            fLeaf acc leafInfo
        InternalNode nodeInfo subTrees ->
            foldl fold' (fNode acc nodeInfo) subTrees


mapTree :: (tLeaf -> tLeaf') -> (tNode -> tNode') -> Tree tLeaf tNode -> Tree tLeaf' tNode'
mapTree fLeaf fNode = mapTree' where
    mapTree' (LeafNode leafInfo)              = LeafNode $ fLeaf leafInfo
    mapTree' (InternalNode nodeInfo subTrees) =
        InternalNode (fNode nodeInfo) (map mapTree' subTrees)
