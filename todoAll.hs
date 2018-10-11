import System.Environment
import System.Directory
import System.IO
import Data.List 
import Control.Exception

dispatch :: String -> [String] -> IO ()
dispatch "add" = add
dispatch "view" = view 
dispatch "remove" = remove 
dispatch "bump"	= bump
dispatch command = doesntExist command

doesntExist :: String -> [String] -> IO ()
doesntExist command _ = putStrLn $ "The " ++ command ++ " command doesn't exist!"

main = do 
	ls <- getArgs
	if null ls
		then putStrLn "Wrong inputs"
		else do
			let command = head ls
			let arglist = tail ls 
			dispatch command arglist
	

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")
add _ = putStrLn "The add command takes exactly two arguments."

view :: [String] -> IO () 
view [fileName] = do 
	contents <- readFile fileName 
	let todoTasks = lines contents 
	let	numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) 
						[0..] todoTasks
	putStr $ unlines numberedTasks
view _ = putStrLn "The view command takes exactly one arguments."


remove :: [String] -> IO ()
remove [fileName, numberString] = do 
	contents <- readFile fileName
	let todoTasks = lines contents 
	let	numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) 
								[0..] todoTasks 
	putStrLn "These are your previous TO-DO items:"
	mapM_ putStrLn numberedTasks 
	let number = read numberString 
	let	newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
	bracketOnError (openTempFile "." "temp")
	 	(\(tempName, tempHandle) -> do 
			hClose tempHandle
			removeFile tempName) 
		(\(tempName, tempHandle) -> do
			hPutStr tempHandle newTodoItems 
			hClose tempHandle 
			removeFile "todo.txt" 
			renameFile tempName "todo.txt")
remove _ = putStrLn "The remove command takes exactly two arguments."

bump :: [String] -> IO () 
bump [fileName, numberString] = do 
	contents <- readFile fileName 
	let todoTasks = lines contents 
	let numberedTasks = zipWith (\n line -> show n ++ " - " ++ line)
						[0..] todoTasks
	putStrLn "These are your TO-DO items:"
	mapM_ putStrLn numberedTasks 
	let number = read numberString
	let newTodoItems = unlines $ (todoTasks !! number) : delete (todoTasks !! number) todoTasks
	bracketOnError (openTempFile "." "temp")
	 	(\(tempName, tempHandle) -> do 
			hClose tempHandle
			removeFile tempName) 
		(\(tempName, tempHandle) -> do
			hPutStr tempHandle newTodoItems 
			hClose tempHandle 
			removeFile "todo.txt" 
			renameFile tempName "todo.txt")
bump _ = putStrLn "The bump command takes exactly two arguments."