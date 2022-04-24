{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Chapter6 where

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

leaf :: a -> Tree a
leaf x = Branch x Empty Empty

-- Problem 55 --
cbalTrees :: Int -> [Tree Char]
cbalTrees 1 = [Branch 'x' Empty Empty]
cbalTrees n 
  | even n    = [Branch 'x' l r | l <- cbalTrees $ n `div` 2
                                , r <- cbalTrees $ n `div` 2]
  | otherwise = [Branch 'x' l r | l <- cbalTrees $ (n - 1) `div` 2
                                , r <- cbalTrees $ (n + 1) `div` 2]  ++
                [Branch 'x' r l | l <- cbalTrees $ (n - 1) `div` 2
                                , r <- cbalTrees $ (n + 1) `div` 2]  


