module FP99 where

import Data.List (group)

-- p01
last01 :: [p] -> p
last01 [h] = h
last01 (_ : t) = last01 t
last01 [] = error "Cannot get last of empty list"

--p02
penultimate :: [p] -> p
penultimate [] = error "Cannot get penultimate of empty list"
penultimate [x] = error "Cannot get penultimate of singleton list"
penultimate [x, _] = x
penultimate (_ : xs) = penultimate xs

--p03
elementAt :: (Eq t, Num t) => [p] -> t -> p
elementAt [] _ = error "too few elements"
elementAt (x : _) 0 = x
elementAt (_ : xs) n = elementAt xs (n - 1)

--p04
myLength :: Num p => [a] -> p
myLength xs = myLengthInternal xs 0
  where
    myLengthInternal [] n = n
    myLengthInternal (_ : xs) n = myLengthInternal xs (n + 1)

myLength1 :: (Foldable t, Num n) => t a -> n
myLength1 = foldl (\n _ -> n + 1) 0

myLength2 :: [b] -> Integer
myLength2 = fst . last . zip [1 ..]

--p05
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

myReverse2 :: [a] -> [a]
myReverse2 = foldl (flip (:)) []

--p06
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == myReverse2 xs

--p07
data NestedList a = Elem a | List [NestedList a]

myFlatten :: NestedList a -> [a]
myFlatten (Elem a) = [a]
myFlatten (List xs) = concatMap myFlatten xs

--p08
compress :: Eq a => [a] -> [a]
compress [] = []
compress [a] = [a]
compress (x : y : zs)
  | x == y = compress (x : zs)
  | otherwise = compress (y : zs)

compress1 :: Eq a => [a] -> [a]
compress1 (x : ys@(y : _))
  | x == y = compress ys
  | otherwise = x : compress ys
compress1 ys = ys

compress2 :: Eq a => [a] -> [a]
compress2 = map head . group

--p09
pack :: Eq a => [a] -> [[a]]
pack = group

pack1 :: Eq a => [a] -> [[a]]
pack1 = reverse . foldl reducer [[]]
  where
    reducer [[]] a = [[a]]
    reducer acc@(xs@(x : _) : rest) a
      | x == a = (a : xs) : rest
      | otherwise = [a] : acc

--p10
encode :: Eq b => [b] -> [(Int, b)]
encode = map (\xs -> (length xs, head xs)) . pack

encode1 :: Eq b => [b] -> [(Int, b)]
encode1 l = [(length xs, head xs) | xs <- group l]

--p11
data Encoded a = Single a | Multiple Int a deriving (Show)

mapEncoding :: (Int, a) -> Encoded a
mapEncoding (1, a) = Single a
mapEncoding (n, a) = Multiple n a

encodeMod :: Eq a => [a] -> [Encoded a]
encodeMod = map mapEncoding . encode

encodeMod1 :: Eq a => [a] -> [Encoded a]
encodeMod1 l = [mapEncoding e | e <- encode l]