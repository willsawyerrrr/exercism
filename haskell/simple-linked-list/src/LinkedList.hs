module LinkedList
  ( LinkedList,
    datum,
    fromList,
    isNil,
    new,
    next,
    nil,
    reverseLinkedList,
    toList,
  )
where

data LinkedList a = Nil | Foo a (LinkedList a) deriving (Eq, Show)

datum :: LinkedList a -> a
datum (Foo x _) = x

fromList :: [a] -> LinkedList a
fromList = foldr Foo Nil

isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _ = False

new :: a -> LinkedList a -> LinkedList a
new = Foo

next :: LinkedList a -> LinkedList a
next (Foo _ l) = l

nil :: LinkedList a
nil = Nil

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList l = fromList $ reverse $ toList l

toList :: LinkedList a -> [a]
toList Nil = []
toList (Foo x l) = x : toList l
