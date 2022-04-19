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
  | n == 0    = acc 
  | otherwise = repliHelp (n-1) e acc ++ [e]

repli :: [a] -> Int -> [a] 
repli xs n = concatMap (\x -> repliHelp n x []) xs

-- Problem 16 -- 
dropEveryIter :: [a] -> Int -> Int -> [a] -> [a]
dropEveryIter xs n k acc 
    | k < 0                 = acc
    | k `mod` n == 0        = dropEveryIter (tail xs) n (k - 1) acc 
    | k `mod` n /= 0        = dropEveryIter (tail xs) n (k - 1) acc ++ [head xs]

dropEvery :: [a] -> Int -> [a] 
dropEvery xs n = dropEveryIter (reverse xs) n (length xs) [] 

-- Problem 17 -- 
split :: [a] -> Int -> ([a], [a])
split xs n = splitHelper xs n [] where
  splitHelper (y:ys) k init
    | k == 1      = (init ++ [y], ys)
    | otherwise   = splitHelper ys (k - 1) (init ++ [y])

-- Problem 18 -- 
slice :: [a] -> Int -> Int -> [a] 
slice xs l r = reverse $ sliceHelper xs 1 [] where
  sliceHelper (y:ys) k acc 
    | k < l             = sliceHelper ys (k + 1) acc
    | k >= l && k < r   = sliceHelper ys (k + 1) (y:acc) 
    | k == r            = (y:acc)

-- Problem 19 -- 

