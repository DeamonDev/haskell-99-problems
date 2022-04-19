{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

-- Problem 21 -- 
insertAt :: a -> [a] -> Int -> [a] 
insertAt e xs n = insertAtHelp e xs 1 [] where
  insertAtHelp e (y:ys) k acc 
    | k < n       = insertAtHelp e ys (k + 1) (acc ++ [y])
    | k == n      = (acc ++ [e, y] ++ ys)

-- Problem 22 -- 
