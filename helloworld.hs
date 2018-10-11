main = do
 _ <- putStrLn "hello, world, what's your name"
 name <- getLine
 putStrLn ("Hey " ++ name ++ ", you rsture!")