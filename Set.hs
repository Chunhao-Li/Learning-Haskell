module Set (Set ,
    empty       ,
    sing        ,
    memSet      ,
    union,inter,diff    ,
    eqSet       ,
    subSet,
    makeSet,
    filterSet,
    mapSet,
    -- foldSet,
    -- showSet,
    -- card,
    -- flatten
    ) where

import Data.List hiding (union)

newtype Set a = Set [a] deriving Show

instance Eq a => Eq (Set a) where
    (==) = eqSet

instance Ord a => Ord (Set a) where
    (<=) = leqSet

eqSet :: Eq a => Set a -> Set a -> Bool
eqSet (Set xs) (Set ys) = (xs == ys)

leqSet :: Ord a => Set a -> Set a -> Bool
leqSet (Set xs) (Set ys) = (xs <= ys)

empty :: Set a
empty = Set []

sing :: a -> Set a
sing x = Set [x]

memSet :: Ord a => Set a -> a -> Bool
memSet (Set []) _ = False
memSet (Set (x:xs)) y
    | x < y         = memSet (Set xs) y
    | x == y        = True
    | otherwise     = False

inter :: Ord a => Set a -> Set a -> Set a
inter (Set xs) (Set ys) = Set (int xs ys)

int :: Ord a => [a] -> [a] -> [a] 
int [] ys = ys 
int xs [] = xs 
int (x:xs) (y:ys) 
  |  x < y = int xs (y:ys)
  | x == y = x:int xs ys 
  | otherwise = int (x:xs) ys
union :: Ord a => Set a -> Set a -> Set a
union (Set xs) (Set ys) = Set (uni xs ys)

uni :: Ord a => [a] -> [a] -> [a]
uni [] ys = ys 
uni xs [] = xs 
uni (x:xs) (y:ys)
  | x < y = x:uni xs (y:ys)
  | x == y = x:uni xs ys
  | otherwise = y:uni (x:xs) ys

diff :: Ord a => Set a -> Set a -> Set a
diff (Set xs) (Set ys) = Set (dif xs ys)

dif :: Ord a => [a] -> [a] -> [a] 
dif [] _ = []
dif xs [] = xs 
dif (x:xs) (y:ys)
  | x < y       = x: dif xs (y:ys)
  | x == y      = dif xs ys 
  | otherwise   = dif (x:xs) ys

subSet :: Ord a => Set a -> Set a -> Bool
subSet (Set xs) (Set ys) = subS xs ys 

subS :: Ord a => [a] -> [a] -> Bool 
subS [] _ = True
subS _ [] = False 
subS (x:xs) (y:ys)
  | x < y   = False 
  | x == y  = subS xs ys 
  | otherwise = subS (x:xs) ys

mapSet :: Ord b => (a -> b) -> Set a -> Set b 
mapSet f (Set xs) = makeSet (map f xs)

filterSet :: Ord a => (a -> Bool) -> Set a -> Set a
filterSet f (Set xs) = makeSet $ filter f xs 


makeSet :: Ord a => [a] -> Set a
makeSet = Set . remDups . sort
          where
            remDups []  = []
            remDups [x] = [x]
            remDups (x:y:xs)
              | x < y     = x: remDups (y:xs)
              | otherwise = remDups (y:xs)





