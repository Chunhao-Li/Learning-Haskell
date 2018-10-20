module TestC where

func :: Int -> Int 
func x = x+1

data Player = Dark | Light deriving (Show, Eq)
type Board = [[Maybe Player]]