module TestB where

import TestC
funB :: Int -> Int
funB x = x*100

data Game = Game (Maybe Player) Board
data Tree a = Node a [Tree a]