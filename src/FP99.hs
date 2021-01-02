module FP99 where

import Data.List (group)

--p01
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

--p12
decodeMod :: [Encoded a] -> [a]
decodeMod = concatMap decodeHelper
  where
    decodeHelper (Single a) = [a]
    decodeHelper (Multiple n a) = replicate n a

--p13
encodeDirect :: Eq a => [a] -> [Encoded a]
encodeDirect [] = []
encodeDirect (a : rest) =
  let encodeRun a n =
        case n of
          1 -> Single a
          _ -> Multiple n a
      helper [] b n
        | n == 0 = []
        | otherwise = [encodeRun b n]
      helper [a] b n
        | n == 0 = helper [] a 1
        | a == b = helper [] b (n + 1)
        | otherwise = encodeRun b n : helper [] a 1
      helper l@(a : rest) b n
        | a == b = helper rest a (n + 1)
        | otherwise = encodeRun b n : encodeDirect l
   in helper rest a 1

encodeDirect2 :: Eq a => [a] -> [Encoded a]
encodeDirect2 =
  let mapper (1, x) = Single x
      mapper (n, x) = Multiple n x

      reducer x [] = [(1, x)]
      reducer x (y@(a, b) : ys)
        | x == b = (1 + a, x) : ys
        | otherwise = (1, x) : y : ys

      encoder = foldr reducer []
   in map mapper . encoder

--p14
dupli :: [a] -> [a]
dupli [] = []
dupli (x : xs) = x : x : dupli xs

dupli2 :: [a] -> [a]
dupli2 = foldr (\x -> (++) [x, x]) []

--p15
repli :: (Num n, Enum n) => [a] -> n -> [a]
repli [] _ = []
repli l n = concatMap helper l
  where
    helper a = map (const a) [1 .. n]

--p16
drop' :: Integral b => [a] -> b -> [a]
drop' xs n = map fst $ filter (\(_, i) -> i `mod` n /= 0) $ zip xs [1 ..]

--p17
split :: (Eq n, Num n) => [a] -> n -> ([a], [a])
split as n = splitHelper as n []
  where
    splitHelper [] _ as = ([], as)
    splitHelper xs 0 as = (reverse as, xs)
    splitHelper (x : xs) n as = splitHelper xs (n - 1) (x : as)

split' :: Integral n => [a] -> n -> ([a], [a])
split' xs n = foldr (\(x, i) (as, bs) -> if i > n then (as, x : bs) else (x : as, bs)) ([], []) (zip xs [0 ..])
