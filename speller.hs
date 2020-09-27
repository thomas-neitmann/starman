speller :: [String] -> String
speller words = enumerate (map isFor words)
  where isFor word = head word : (" is for " ++ word)

enumerate :: [String] -> String
enumerate []         = []
enumerate (hd:nk:[]) = hd ++ ", and " ++ nk
enumerate (hd:tl)    = hd ++ ", " ++ (enumerate tl)