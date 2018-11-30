import System.Random
import Control.Monad(when)

main = do 
	gen <- getStdGen 
	askForNumber gen 

askForNumber :: StdGen -> IO ()
askForNumber gen = do 
	let (randNumber, newGen) = randomR (1,10) gen :: (Int, StdGen)
	putStrLn "Which number in the range from 1 to 10 am I thinking of?"
	numberString <- getLine 
	when (not $ null numberString) $ do 
		let number = reads numberString :: [(Int, String)]
		if number == []
			then putStrLn "Parse error"
			else if randNumber == (fst $ head number )
				then putStrLn "You are correct!"
				else putStrLn $ "Sorry, it was " ++ show randNumber
		askForNumber newGen