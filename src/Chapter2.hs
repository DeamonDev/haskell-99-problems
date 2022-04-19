{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
import Chapter1
import Control.Arrow

-- Problem 11 --
data Entity a = Multiple Int a | Single a deriving (Show)
encodeModified :: (Eq a) => [a] -> [Entity a]
encodeModified xs =
  map encodeModifiedMap $ encode xs
  where
    encodeModifiedMap (n, x)
      | n == 1       = Single x
      | otherwise    = Multiple n x

-- Problem 12 -- 
decodeModified :: (Eq a) => [Entity a] -> [a]
decodeModified =
  foldr decodeModifiedFold []
  where
    decodeModifiedFold (Single x) acc     = x:acc
    decodeModifiedFold (Multiple n x) acc = replicate n x ++ acc

-- Problem 13 -- 
encodeDirect :: (Eq a) => [a] -> [Entity a]
encodeDirect xs = encodeDirectHelper 1 (head xs) (tail xs) where
  encodeDirectHelper n x []
    | n == 1 = [Single x]
    | otherwise = [Multiple n x]
  encodeDirectHelper 1 x (y:ys)
    | x == y     = encodeDirectHelper 2 x ys
    | otherwise  = (Single x):(encodeDirectHelper 1 y ys)
  encodeDirectHelper n x (y:ys)
    | x == y     = encodeDirectHelper (n + 1) x ys
    | otherwise  = (Multiple n x):(encodeDirectHelper 1 y ys)

-- Problem 14 --
dupli :: [a] -> [a]
dupli = concatMap (\x -> [x, x]) 

-- Problem 15 --
repliHelp :: Int -> a -> [a] -> [a] 
repliHelp n e acc 
  | n == 0 = acc 
  | otherwise = repliHelp (n-1) e acc ++ [e]

repli :: [a] -> Int -> [a] 
repli xs n = concatMap (\x -> repliHelp n x []) xs

-- Problem 16 -- 