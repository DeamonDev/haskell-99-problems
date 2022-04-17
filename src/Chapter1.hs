-- Problem 1 --
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

myLast :: [a] -> a
myLast []     = error "there is no last element in an empty list!"
myLast [x]    = x
myLast (x:xs) = myLast xs

-- Problem 2 -- 
myButLast :: [a] -> a
myButLast []      = error "there is no but last element in an empty list!"
myButLast [x]     = error "there is no but last element in an singleton list!"
myButLast [x, y]  = x
myButLast (x:xs)  = myButLast xs

-- Problem 3 --
elementAt :: [a] -> Int -> a
elementAt _ k
  | k < 1          = error "index out of range"
elementAt (x:_) 1  = x
elementAt [] _     = error "index out of range"
elementAt (_:xs) k = elementAt xs (k - 1)

-- Problem 4 --
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + length xs

-- Problem 5 --
myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

-- Problem 6 -- 
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == myReverse xs

-- Problem 7 -- 
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x)      = [x]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List [])     = []

-- Problem 8 --
compress :: (Eq a) => [a] -> [a]
compress xs =
  reverse $ foldr (\x a -> if x == last a then a else a ++ [x]) [last xs] xs

compress' :: (Eq a) => [a] -> [a]
compress' = foldr compressLambda []
  where compressLambda x [] = [x]
        compressLambda x acc
          | x == head acc = acc
          | otherwise     = x:acc

-- Problem 9 -- 
pack :: (Eq a) => [a] -> [[a]]
pack = foldr packLambda [[]]
  where packLambda x [[]] = [[x]]
        packLambda x acc@(y:ys) =
          if x == head y then (x:y):ys else [x]:acc


