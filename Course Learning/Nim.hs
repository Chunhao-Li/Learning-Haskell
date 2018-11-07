-- Oct 9th
-- Author: Frederick Li
-- module Nim where


-- represent a state of a game
data Player = Max | Min
              deriving (Show, Eq)

data Nim = Nim { turn :: Player, npebbles :: Int }
                        deriving (Show)


data Tree a = Node a [Tree a]

instance Show a => Show (Tree a) where
 show tree = pretty tree
     where
         pretty :: Show a => Tree a -> String
         pretty = unlines . layout
         layout :: Show a => Tree a -> [String]
         layout (Node v []) = [show v]
         layout (Node v children) = [show v] ++
                concat (map indent (map layout children))
         indent :: [String] -> [String]
         indent = map (" "++)

class Position a where
    player :: a -> Player
    moves :: a -> [a] -- given a position, output legal moves
    won :: a -> Player -> Bool -- checking whether the player won


instance Position Nim where
    player a = turn a
    won a p = npebbles a == 0 && p == turn a
    moves a
        | score >= 3 = [ Nim next (score - 1),
                         Nim next (score - 2),
                         Nim next (score - 3)]
        | score >= 2 = [ Nim next (score - 1),
                         Nim next (score - 2)]
        | score >= 1 = [ Nim next (score - 1)]
        | otherwise = []

        where
            score = npebbles a
            next = (other.turn) a
            other :: Player -> Player
            other Min = Max
            other Max = Min

gameTree :: (Position a) => (a -> [a]) -> a -> Tree a
gameTree f a = Node a (map (gameTree f) (f a))

-- value :: (Position a) => a -> Int
-- value pos
--  | won pos Max = 1
--  | won pos Min = -1 -- losing positio n for Max
--  | player pos == Max = maximum (map value (moves pos))
--  | player pos == Min = minimum (map value (moves pos))
--  | otherwise = error "Something is wrong!"

value :: (Position a) => a -> Int
value pos
    | won pos Max = 1
    | won pos Min = -1 -- losing position for Max
    | player pos == Max  = maximum (map value (moves pos))
    | player pos == Min  = minimum (map value (moves pos))
    | otherwise = error "Something's wrong!"


staticValue :: (Position a) => a -> Int
staticValue pos
   | won pos Max = 1
   | won pos Min = -1
   | otherwise   = 0

nimGameTree :: (Position a) => a -> Tree a
nimGameTree = gameTree moves

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f node@(Node a lst) = case lst of
 []      -> Node (f a) []
 cs      -> Node (f a) (map (treeMap f) cs)

maximise :: (Ord a) => Tree a -> a
maximise (Node v [])  = v
maximise (Node _ subtrees) = maximum (map minimise subtrees)

minimise :: (Ord a) => Tree a -> a
minimise (Node v [])       = v
minimise (Node _ subtrees) = minimum (map maximise subtrees)

prune :: Int -> Tree a -> Tree a
prune _ (Node a []) = Node a []
prune 0 (Node a children) = Node a []
prune n (Node a children) = Node a (map (prune (n-1)) children) 


minimax :: (Position a) => Int -> a -> Int
minimax d  = maximise .treeMap staticValue . prune d . nimGameTree











