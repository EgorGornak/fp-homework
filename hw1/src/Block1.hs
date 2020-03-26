module Block1
  ( nextDay
  , afterDays
  , isWeekend
  , plus
  , multiply
  , difference
  , fromNatToInt
  , fromIntToNat
  , compareNat
  , isEmpty
  , getSize
  , findElement
  , addElement
  , Block1.fromList
  , deleteElement
  , Weekday(..)
  , Tree(..)
  ) where

import Data.List
import Data.List.NonEmpty

data Weekday
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday

-- | get next day
nextDay :: Weekday -> Weekday
nextDay Monday    = Tuesday
nextDay Tuesday   = Wednesday
nextDay Wednesday = Thursday
nextDay Thursday  = Friday
nextDay Friday    = Saturday
nextDay Saturday  = Sunday
nextDay Sunday    = Monday

-- | get day after N days
afterDays :: Weekday -> Int -> Weekday
afterDays day after_count
  | after_count == 0 = day
  | otherwise = afterDays (nextDay day) (after_count - 1)

-- | if weekend return True otherwise return False
isWeekend :: Weekday -> Bool
isWeekend Saturday = True
isWeekend Sunday   = True
isWeekend _        = False

-----------------
data Nat
  = Z
  | S Nat

-- | plus for Nat
plus :: Nat -> Nat -> Nat
plus x Z     = x
plus x (S y) = plus (S x) y

-- | multiply for Nat
multiply :: Nat -> Nat -> Nat
multiply x Z     = Z
multiply x (S Z) = x
multiply x (S y) = plus x (multiply x y)

-- | difference for Nat
difference :: Nat -> Nat -> Nat
difference Z x         = Z
difference x Z         = x
difference (S x) (S y) = difference x y

-- | get Nat return Int
fromNatToInt :: Nat -> Int
fromNatToInt Z     = 0
fromNatToInt (S x) = 1 + fromNatToInt x

-- | get Int return Nat
fromIntToNat :: Int -> Nat
fromIntToNat x
  | x <= 0 = Z
  | otherwise = S (fromIntToNat (x - 1))

-- | compare two Nat's
compareNat :: Nat -> Nat -> Ordering
compareNat Z Z         = EQ
compareNat (S x) Z     = GT
compareNat Z (S x)     = LT
compareNat (S x) (S y) = compareNat x y

-----------------
data Tree a
  = Leaf
  | Node (NonEmpty a) (Tree a) (Tree a)

-- | if tree empty return true otherwise return false
isEmpty :: Tree a -> Bool
isEmpty Leaf = True
isEmpty _    = False

-- | return size of tree
getSize :: Tree a -> Int
getSize Leaf                        = 1
getSize (Node _ leftTree rightTree) = 1 + getSize leftTree + getSize rightTree

-- | if element in tree return true otherwise return false
findElement :: Ord a => Tree a -> a -> Bool
findElement Leaf _ = False
findElement (Node list leftTree rightTree) element
  | Data.List.NonEmpty.head list == element = True
  | Data.List.NonEmpty.head list < element = findElement leftTree element
  | otherwise = findElement rightTree element

-- | add element in tree
addElement :: Ord a => Tree a -> a -> Tree a
addElement Leaf element = Node (Data.List.NonEmpty.insert element []) Leaf Leaf
addElement (Node list leftTree rightTree) element
  | Data.List.NonEmpty.head list == element = Node (Data.List.NonEmpty.insert element list) leftTree rightTree
  | Data.List.NonEmpty.head list < element = Node list (addElement leftTree element) rightTree
  | otherwise = Node list leftTree (addElement rightTree element)

-- | get tree from list
fromList :: Ord a => [a] -> Tree a
fromList []     = Leaf
fromList (x:xs) = addElement (Block1.fromList xs) x

toList :: Ord a => Tree a -> [a]
toList Leaf = []
toList (Node list leftTree rightTree) =
  Block1.toList leftTree ++ Data.List.NonEmpty.toList list ++ Block1.toList rightTree

-- | delete element from tree
deleteElement :: Ord a => Tree a -> a -> Tree a
deleteElement tree element = Block1.fromList (delete element (Block1.toList tree))
