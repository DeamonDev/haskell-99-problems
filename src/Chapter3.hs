{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Chapter3 where

import Chapter2

import System.Random

-- Problem 21 -- 
insertAt :: a -> [a] -> Int -> [a]
insertAt e xs n = insertAtHelp e xs 1 [] where
  insertAtHelp e (y:ys) k acc
    | k < n       = insertAtHelp e ys (k + 1) (acc ++ [y])
    | k == n      = (acc ++ [e, y] ++ ys)

-- Problem 22 -- 
myRange :: Int -> Int -> [Int]
myRange l r = reverse $ myRangeHelper l [] where
  myRangeHelper x acc
    | x <= r    = myRangeHelper (x + 1) (x:acc)
    | otherwise = acc

-- Problem 23 -- 
randomlySelect :: (Show a) => Int -> [a] -> IO ()
randomlySelect n xs = do
  gen         <- getStdGen
  let randIndexes = take n (randomRs (0, (length xs - 1)) gen)
  let randomVals = map (xs !!) randIndexes
  print randomVals

-- Problem 24 --
diffSelect :: Int -> Int -> IO ()
diffSelect n bound = randomlySelect n [1..bound]

