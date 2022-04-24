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


-- Problem 56 -- 
mirror :: Tree a -> Tree a -> Bool
mirror Empty              Empty            = True
mirror (Branch _ l r)     (Branch _ l' r') = (mirror l l') && (mirror r r')
mirror (Branch {} )       Empty            = False
mirror Empty              (Branch {})      = False

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ l r) = mirror l r

-- Problem 57 -- 

