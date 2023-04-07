module BST
  ( BST,
    bstLeft,
    bstRight,
    bstValue,
    empty,
    fromList,
    insert,
    singleton,
    toList,
  )
where

data BST a = Empty | Tree (BST a) a (BST a) deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft Empty = Nothing
bstLeft (Tree left _ _) = Just left

bstRight :: BST a -> Maybe (BST a)
bstRight Empty = Nothing
bstRight (Tree _ _ right) = Just right

bstValue :: BST a -> Maybe a
bstValue Empty = Nothing
bstValue (Tree _ val _) = Just val

empty :: BST a
empty = Empty

fromList :: Ord a => [a] -> BST a
fromList [] = Empty
fromList (x : xs) = foldl (\tree val -> insert val tree) (singleton x) xs

insert :: Ord a => a -> BST a -> BST a
insert x Empty = Tree Empty x Empty
insert x (Tree left val right)
  | x > val = Tree left val (insert x right)
  | otherwise = Tree (insert x left) val right

singleton :: a -> BST a
singleton x = Tree Empty x Empty

toList :: BST a -> [a]
toList Empty = []
toList (Tree left a right) = toList left ++ [a] ++ toList right
