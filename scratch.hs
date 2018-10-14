module Scratch 
( Section(..)
, RoadSystem
, Label(..)
, Path
, roadStep
, groupOf
,optimalPath) where
--Pal week05 isPangram
import Data.List
import Data.Char
import qualified Data.Map as Map
import System.Random
import qualified Data.ByteString.Lazy as B 
import qualified Data.ByteString as S 

check ::  (Eq a) => [a] -> [a] -> [a]
check a b 
	| b == [] = b
	| a == [] = b
	| otherwise = check (tail a ) (quickremove b (head a))

quickremove :: (Eq a) => [a] -> a -> [a] 
quickremove ls x
	| ls == [] = []
	| head ls == x = tail ls
	| otherwise = (head ls) : (quickremove (tail ls) x)

alphabet :: String
alphabet = "abcdefghijklmnopqrstuvwxyz"

isPangram :: String->Bool
isPangram str = case (check str alphabet) of
				[]			-> True
				_			-> False


sumOdd :: [Integer] -> Integer
sumOdd xs = sum (filter (odd) xs)	 


oddCubes :: [Integer] -> [Integer]
oddCubes ls = case ls of 
		[]	->	[]
		_	-> map (^3) (filter odd ls)



addUpToZero :: [Integer] -> Bool
addUpToZero ls = case ls of 
    []                   -> False
    (x:xs) 
        | x == 0   -> True
        | (-x) `elem` xs  -> True
        | otherwise		-> addUpToZero xs

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

product' :: (Num a) => [a] -> a
product' = foldl (*) 1

and' :: [Bool] -> Bool
and' = foldr (&&) True 

and'' :: [Bool] -> Bool
and'' = foldl (&&) True

oddSquareSum :: Integer
oddSquareSum = sum. takeWhile (<1000). filter odd $ map (^2)$ [1..]

wordNums :: String -> [(String, Int)]
wordNums = map (\ws -> (head ws, length ws)). group. sort. words

isIn :: Eq a => [a] -> [a] -> Bool
needle `isIn` haystack = any (isPrefixOf needle) (tails haystack)

encode :: Int -> String -> String
encode offset= map (chr.(+ offset).ord)

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

digitSum :: Int -> Int
digitSum = sum. map digitToInt . show

firstTo40 :: Maybe Int
firstTo40 = find (\x -> digitSum x == 40) [0..] 

firstToNum :: Int -> Maybe Int
firstToNum n = find (\x -> digitSum x == n) [0..] 

findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey _ [] = Nothing
findKey key ((k,v):xs)
	| key == k = Just v
	| otherwise = findKey key xs


findKey1 :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey1 key xs = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing xs

len' :: [a] -> Int -> Int
len' [] acc = acc
len' (x:xs) acc = len' xs (1 + acc)

data Shape = Circle Float Float Float | Rectangle Float Float Float Float
area :: Shape -> Float
area (Circle _ _ r) = pi*r^2
area (Rectangle x1 y1 x2 y2) = (abs $ x2-x1) * (abs $ y2 - y1)
		
data Person = Person {firstName :: String, lastName :: String, age :: Int} deriving (Eq,Show,Read)
mikeD = Person {firstName = "Michael", lastName = "Diamond", age=43}
mysteryDude = "Person {firstName =\"Michael\"" ++", lastName = \"Diamond\" " ++ ", age = 43}"

data DayNames = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq, Enum, Show, Bounded, Ord, Read)


inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook

type Name = String
type PhoneNumber = String
type PhoneBook = [(Name,PhoneNumber)]

type AssocList k v = [(k,v)]

--data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)


data LockerState = Taken | Free deriving (Show,Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of 
	Nothing -> Left $ "Locker" ++ show lockerNumber ++ " doesn't exist!"
	Just (state, code) -> case state of 
							Taken	-> Left $ "Locker" ++ show lockerNumber ++ " is already taken!"
							Free	-> Right code

lockers :: LockerMap
lockers = Map.fromList [(100,(Taken,"dfsf")), (101,(Free,"fse")),(103,(Free,"Idfw"))]

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
	| x < a 	= Node a (treeInsert x left) right
	| x > a 	= Node a left (treeInsert x right)
	| otherwise = Node a left right

treeElem :: (Ord a) => a -> Tree a -> Bool 
treeElem x EmptyTree = False 
treeElem x (Node a left right)
	| x < a 	= treeElem x left 
	| x > a 	= treeElem x right
	| x == a 	= True

data TrafficLight = Red | Yellow | Green
instance Eq TrafficLight where
	Red == Red = True
	Yellow == Yellow = True
	Green == Green = True
	_ == _ = False

instance Show TrafficLight where
	show Red = "Red light"
	show Yellow = "Yellow light"
	show Green = "Green light"

class YesNo a where
	yesno :: a -> Bool

instance YesNo Int where
	yesno 0 = False
	yesno _ = True

instance YesNo [a] where
	yesno [] = False
	yesno _ = True

instance YesNo Bool where
	yesno = id 

instance YesNo (Maybe a) where 
	yesno (Just _) = True
	yesno Nothing = False

instance YesNo (Tree a) where
	yesno EmptyTree = False
	yesno _ =True

instance YesNo TrafficLight where
	yesno Red = False
	yesno _ = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = 
	if yesno yesnoVal
		then yesResult
		else noResult

reorder :: [a] -> [a]				-- The same as reverse
reorder  = foldl (flip (:)) []

fib :: Integer -> Integer 
fib 0 = 0
fib 1 = 1 
fib n = fib (n-1) + fib (n-2)

--fiblist :: [Integer]
--fiblist = unfoldright go 
	--where  
	--	go (a,b) = Just (a,(b,a+b))

unfoldright :: (b -> Maybe (a,b)) -> b -> [a]
unfoldright f b = case f b of 
	Just (a, b')	-> a : unfoldright f b' 
	Nothing			-> []

fiblist2 = 0 : 1 : zipWith (+) fiblist2 (tail fiblist2)

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen = 
	let (firstCoin, newGen) = random gen;
		(secondCoin, newGen') = random newGen;
		(thirdCoin, newGen'') = random newGen'
	in (firstCoin, secondCoin, thirdCoin)

--finiteRandoms :: (RandomGen g, Random a, Num n) => n -> g -> ([a], g)
--finiteRandoms 0 gen = ([], gen)
--finiteRandoms n gen = 
--	let (value, newGen) = random gen;
--		(restOfList, finalGen) = finiteRandoms (n-1) newGen

isPrime :: Integer -> Bool 
isPrime n = foldr (&&) True $ map ((/=0).(mod n)) [2..(n-1)]

solveRPN :: String -> Double
solveRPN = head . foldl foldingFunction [] . words
	where 
		foldingFunction (x:y:ys) "*"	= (y * x) : ys 
		foldingFunction (x:y:ys) "-"	= (y - x) : ys 
		foldingFunction (x:y:ys) "+"	= (y + x) : ys 
		foldingFunction (x:y:ys) "/"	= (y / x) : ys 
		foldingFunction (x:y:ys) "^"	= (y ** x) : ys 
		foldingFunction (x:xs) "ln"	= log x : xs 
		foldingFunction xs "sum"	= [sum xs]
		foldingFunction xs numberString	= read numberString:xs 

data Section = Section {getA :: Int, getB :: Int, getC :: Int} deriving (Show)
type RoadSystem = [Section]

heathrowToLondon :: RoadSystem
heathrowToLondon = [Section 50 10 30,
					Section 5 90 20,
					Section 40 2 25,
					Section 10 8 0]

data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]

optimalPath :: RoadSystem -> Path
optimalPath roadSystem = 
	let (bestAPath, bestBPath) = foldl roadStep ([], []) roadSystem
	in if sum (map snd bestAPath) <= sum (map snd bestBPath)
		then reverse bestAPath
		else reverse bestBPath

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) = 
	let timeA = sum (map snd pathA) ;
		timeB = sum (map snd pathB);
		forwardtimeToA = timeA + a ;
		crosstimeToA = timeB + b + c;
		forwardtimeToB = timeB + b;
		crosstimeToB = timeB + a + c; 
		newPathToA = if forwardtimeToA <= crosstimeToA
						then (A, a) : pathA 
						else (C, c) : (B, b) :pathB;
		newPathToB = if forwardtimeToB <= crosstimeToB
						then (B, b) : pathB 
						else (C, c) : (A, a) :pathA
	in (newPathToA, newPathToB)

groupOf :: Int -> [a] -> [[a]]
groupOf 0 _ = undefined
groupOf _ [] = [] 
groupOf n xs = take n xs : groupOf n (drop n xs)

type Board = [Move]
type Move = (Player, (Int, Int)) -- X, Y coordinates
data Player = X | O
    deriving (Show, Eq)
    
-- Returns a number on how good a board is, bigger is better
evaluateBoard :: Board -> Int
evaluateBoard = undefined
-- Assuming the move is valid, make the move and returns the new board
makeNextMove :: Board -> Move -> Board
makeNextMove = undefined
-- Returns true if a move is able to be made in this board
isValidMove :: Board -> Move -> Bool
isValidMove = undefined



greedyMove :: Board -> Player -> Move
greedyMove = undefined

allMoves :: Player -> [Move]
allMoves player = [(player, (x, y)) | x <- [0..2], y <- [0..2]]

validMoves :: Board -> [Move] -> [Move]
validMoves board moves 
	| length board == 9		= []
	| otherwise  			= filter (isValidMove board) moves

scoreMove :: Board -> [Move] -> [(Int, Move)]
scoreMove board possibleMoves = undefined

maximum' :: [(Int, Move)] -> Move
maximum' scoredMoves =undefined


qSort :: [Integer] -> [Integer]
qSort [] = []
qSort (x:xs) = qSort [y | y<-xs, y<=x] ++ [x] ++ qSort [y | y<-xs, y>x]

mSort :: [Integer] -> [Integer]
mSort [] = []
mSort [x] = [x]
mSort lst = merge (mSort firsthalf) (mSort secondhalf)
	where 
		firsthalf = take half lst
		secondhalf = drop half lst 
		half 	= (length lst) `div` 2

merge :: [Integer] -> [Integer] -> [Integer]
merge lst [] = lst 
merge [] lst = lst 
merge (x:xs) (y:ys)
	| x <= y 		= x:merge xs (y:ys)
	| otherwise		= y:merge (x:xs) ys





data CMaybe a = CNothing | CJust Int a deriving (Show)

instance Functor CMaybe where
	fmap f CNothing = CNothing
	fmap f (CJust counter x) = CJust (counter+1) (f x)


-- instance Applicative Maybe where
-- 	pure = Just
-- 	Nothing <*> _ = Nothing
-- 	(Just f) <*> something = fmap f something

newtype CharList a = CharList { getcharlist :: [Char]} deriving (Show, Eq)


newtype Pair b a = Pair {getPair :: (a, b)}
instance Functor (Pair c) where 
	fmap f (Pair (x, y)) = Pair (f x, y)




