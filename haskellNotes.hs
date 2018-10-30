-- data Tree a = Null | Node {valTree :: a, lTree:: Tree a, rTree :: Tree a}
data Tree a = Null | Node {lTree:: Tree a, rTree :: Tree a, valTree::a}
data Stack a = Empty | Top a (Stack a)
push ::  Stack a -> a  -> Stack a
push stack elem = Top elem stack

popAll :: Stack a -> [a]
popAll Empty = []
popAll (Top x stack) = x:popAll stack

reverseStack::[a] -> [a]
reverseStack xs = popAll (foldl push Empty xs)

-- | Writing foldl with foldr

--foldr :: (a -> b -> b)->b -> [a] -> b
myfoldl :: (b -> a -> b) -> b -> [a] -> b
myfoldl f zeroL list = foldr step id list $ zeroL
  where step x g a = g (f a x)

-- x: last element of the list
-- g: accumulator with functions
-- a: accumulator

foldl f v []    = v       -- f :: b -> a -> b
foldl f v (x:xs) = foldl f (f v x) xs
foldl f 0 [1,2,3] = foldl f (f 0 1) [2,3]
                  = foldl f (f (f 0 1) 2) [3]
                  = foldl f (f (f (f 0 1) 2) 3) []
                  = f (f (f 0 1) 2) 3

fold :: (a -> b -> b) -> b -> ([a] -> b)
fold funC v []       = v 
fold funC v (x:xs)  = funC x (fold funC v xs)
fold funC 0 [1,2,3]  = funC 1 (fold funC 0 [2,3])
                     = funC 1 (funC 2 (fold funC 0 [3]))
                     = funC 1 (funC 2 (funC 3 (fold funC 0 [])))
                     = funC 1 (funC 2 (funC 3 0))


foldl f acc xs = g xs acc -- f :: b -> a -> b
  where                   -- g :: a -> acc -> acc
    g [] acc = acc          -- g [] = \acc -> acc 
                            -- g [] = id
    g (x:xs) acc = g xs (f acc xs)  -- g (x:xs) = \acc -> g xs (f acc xs)
                                    --          = funC x (g xs) 
                                    --  where g = fold funC acc
                                    -- func :: a -> b -> b
g (x:xs) = funC x (g xs)
g (x:xs) acc = funC x (g xs) acc
g xs (f acc x) = funC x (g xs) acc 
g' (f acc x)   = funC x g' acc 
-- | Change the sides
funC x g' acc = g' (f acc x)
funC = \x g' acc -> g' (f acc x)
     = \x g' -> (\acc -> g' (f acc x))

foldl f acc xs = g xs $ acc 
               = fold funC acc xs $ acc 
               = foldr funC acc xs $ acc 
               = foldr (\x g' -> (\acc -> g' (f acc x))) id xs $ acc 
step  = funC 
step x g a = g (f a x)
step x g a = (\x g -> (\a -> g (f a x)))

