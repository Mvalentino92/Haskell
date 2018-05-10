{-act :: IO (Char,Char)
act = do x <- getChar
         getChar
         y <- getChar
         return (x,y)


getLine' :: IO String
getLine' = do x <- getChar
              if x == '\n' then
                return []
              else
                do xs <- getLine'
                   return (x:xs)

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do putChar x
                    putStr' xs-}


-- Gets the word to be guessed
getWord :: IO String
getWord = do putStr "Enter a word for the opponent to guess: "
             word <- getLine
             return word 

-- Updates the letters guessed correctly and returns the word.
updateWord :: String -> String -> Char -> String
updateWord [] [] _ = ""
updateWord (x:xs) (y:ys) g
           | x == g = x : updateWord xs ys g
           | otherwise = y : updateWord xs ys g


ask :: IO Char
ask = do putStr "What letter would you like to guess?: "
         l <- getChar
         putStrLn ""
         return l

turn :: String -> String -> Int -> IO ()
turn word gword n = do if n < 7 then 
                          do l <- ask
                             let ngword = updateWord word gword l
                             if ngword == word then
                                do putStrLn word
                                   putStrLn "You win!"
                             else
                                do putStr ngword
                                   putStr "   Turns: "
                                   print (n+1) 
                                   turn word ngword (n+1)
                        else 
                           putStrLn "You lose!"

play :: IO ()
play = do 
       word <- getWord
       let gword = replicate (length word) '_'
       putStrLn gword
       turn word gword 0
       return () 
