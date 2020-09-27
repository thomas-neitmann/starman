safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just x

check :: String -> String -> Maybe Char -> (Bool, String)
check word display Nothing = (True, display)
check word display (Just c) =
  let
    new_display = [if x == c then c else y | (x, y) <- zip word display]
  in
    (c `elem` word, new_display)

turn :: String -> String -> Int -> IO()
turn word display n | n == 0          = putStrLn (display ++ "\nYou loose.")
                    | word == display = putStrLn (word ++ "\nYou win!")
                    | otherwise       = mkguess word display n

mkguess :: String -> String -> Int -> IO ()
mkguess word display n = do
  putStrLn (display ++ "  " ++ take n (repeat '*'))
  putStr "Enter your guess: "
  guess <- getLine
  let (correct, display') = check word display (safeHead guess)
  let n' = if correct then n else n - 1
  turn word display' n'

starman :: String -> Int -> IO ()
starman word n = turn word ['_' | _ <- word] n
