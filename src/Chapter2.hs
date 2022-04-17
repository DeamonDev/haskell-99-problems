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






