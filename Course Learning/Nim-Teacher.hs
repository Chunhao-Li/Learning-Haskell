-- Copyright Antony Hosking 2016, Ekaterina Lebedeva 2018.  With inspiration from notes by Dan Licata
-- from CMU 15-150, and reference to Hughes' "Why Functional Programming
-- Matters".
data Player = Max | Min
            deriving (Show, Eq)

-- data type to represent a state of a game 
data Nim = Nim { turn :: Player, npebbles :: Int }
             deriving (Show)

class Position a where
    player :: a -> Player 
    moves :: a -> [a] -- given a position, output legal moves
    won :: a -> Player -> Bool -- checking whether the player won the game 

instance Position Nim where 
    player a = turn a 
    won a p = npebbles a == 0 && p == turn a
    moves a  
       | score >= 3 = [ Nim next (score-1), 
                        Nim next (score-2),
                        Nim next (score-3)]
       | score >= 2 = [ Nim next (score-1), 
                        Nim next (score-2)]
       | score >= 1 = [ Nim next (score-1)]
       | otherwise = []
       where 
         score = npebbles a -- current score of the game (# pebbles in a bank)
         next = (other.turn) a
         other :: Player -> Player
         other Min = Max
         other Max = Min 


value :: (Position a) => a -> Int
value pos 
    | won pos Max = 1
    | won pos Min = -1 -- losing position for Max
    | player pos == Max = maximum (map value (moves pos))
    | player pos == Min = minimum (map value (moves pos))
    | otherwise = error "Something's wrong!"


