module Lib where

import Control.Monad (replicateM)
import Data.List (group, groupBy, nub, sortBy, sortOn, subsequences, tails, union)
import System.Random (newStdGen, randomRs)

-----------LISTS--------------------------------------------------------
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
    splitHelper [] _ as = (as, [])
    splitHelper xs 0 as = (reverse as, xs)
    splitHelper (x : xs) n as = splitHelper xs (n - 1) (x : as)

split' :: Integral n => [a] -> n -> ([a], [a])
split' xs n = foldr (\(x, i) (as, bs) -> if i > n then (as, x : bs) else (x : as, bs)) ([], []) (zip xs [0 ..])

--p18
slice :: Int -> Int -> [a] -> [a]
slice s e = take (e - s + 1) . drop (s - 1)

--p19
rotate xs n = rest ++ start
  where
    cut = if n >= 0 then n else length xs + n
    rest = drop cut xs
    start = take cut xs

--p20
removeAt' :: (Eq t, Num t) => [a] -> t -> (Maybe a, [a])
removeAt' [] _ = (Nothing, [])
removeAt' (x : xs) 1 = (Just x, xs)
removeAt' (x : xs) n = (l, x : xs')
  where
    (l, xs') = removeAt' xs (n - 1)

--p21
insertAt :: (Ord n, Num n) => a -> [a] -> n -> [a]
insertAt x [] _ = [x]
insertAt x l@(a : as) n
  | n <= 1 = x : l
  | otherwise = a : insertAt x as (n - 1)

insertAt' :: (Eq n, Num n) => a -> [a] -> n -> [a]
insertAt' x as n = let (s, e) = split as (n - 1) in s ++ x : e

--p22
range :: Enum a => a -> a -> [a]
range i j = [i .. j]

--p23
rndSelect :: Int -> [a] -> IO [a]
rndSelect n xs = do
  map (xs !!) . take n . nub . randomRs (0, length xs - 1) <$> newStdGen

--p24
rndNumbers :: Int -> Int -> IO [Int]
rndNumbers n m = rndSelect n [0 .. m]

--p25
rndPermutation :: [a] -> IO [a]
rndPermutation xs = rndSelect (length xs) xs

--p26
combinations :: Int -> [a] -> [[a]]
combinations 0 xs = [[]]
combinations n xs =
  [ y : ys
    | y : xs' <- tails xs,
      ys <- combinations (n -1) xs'
  ]

combinations' :: Int -> [a] -> [[a]]
combinations' k ns = filter ((k ==) . length) (subsequences ns)

--p27
combination :: Int -> [a] -> [([a], [a])]
combination 0 xs = [([], xs)]
combination n [] = []
combination n (x : xs) = ts ++ ds
  where
    ts = [(x : ys, zs) | (ys, zs) <- combination (n - 1) xs]
    ds = [(ys, x : zs) | (ys, zs) <- combination n xs]

group' :: [Int] -> [a] -> [[[a]]]
group' [] xs = [[]]
group' (g : gs) xs = concatMap helper $ combination g xs
  where
    helper (as, bs) = map (as :) (group' gs bs)

--p28
lsort :: [[a]] -> [[a]]
lsort = sortOn length

lsort' :: [[a]] -> [[a]]
lsort' = sortBy (\as bs -> compare (length as) (length bs))

lfsort :: [[a]] -> [[a]]
lfsort = concat . lsort . groupBy (\as bs -> length as == length bs) . lsort

-----------LISTS--------------------------------------------------------

-----------ARITHMETIC---------------------------------------------------
--p31
primes :: [Integer]
primes = sieve [2 ..]
  where
    sieve (p : ps) = p : sieve [x | x <- ps, mod x p /= 0]

primesTME = 2 : gaps 3 (join [[p * p, p * p + 2 * p ..] | p <- primes'])
  where
    primes' = 3 : gaps 5 (join [[p * p, p * p + 2 * p ..] | p <- primes'])
    join ((x : xs) : t) = x : union xs (join (pairs t))
    pairs ((x : xs) : ys : t) = (x : union xs ys) : pairs t
    gaps k xs@(x : t)
      | k == x = gaps (k + 2) t
      | otherwise = k : gaps (k + 2) xs

isPrime :: Integral a => a -> Bool
isPrime n = l == 1 || l == 2
  where
    l = length $ filter (\i -> (n `mod` i) == 0) [1 .. n `div` 2]

isPrime' k =
  k > 1
    && foldr
      (\p r -> p * p > k || k `rem` p /= 0 && r)
      True
      primesTME

--p32
gcd' :: Integral t => t -> t -> t
gcd' a b
  | b == 0 = abs a
  | otherwise = gcd' b (a `rem` b)

--p33
coprime :: Integral t => t -> t -> Bool
coprime a b = gcd' a b == 1

-----------ARITHMETIC---------------------------------------------------

-----------TREES--------------------------------------------------------

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

leaf :: a -> Tree a
leaf a = Branch a Empty Empty

-- p55
cbalTrees :: (Eq t, Num t) => t -> [Tree Char]
cbalTrees 0 = [Empty]
cbalTrees 1 = [leaf 'x']
cbalTrees n = nub $ concatMap add $ cbalTrees (n - 1)
  where
    add (Branch x Empty Empty) = [Branch x Empty (leaf 'x'), Branch x (leaf 'x') Empty]
    add (Branch x l Empty) = [Branch x l (leaf 'x')]
    add (Branch x Empty r) = [Branch x (leaf 'x') r]
    add (Branch x l r) = map (\nl -> Branch x nl r) (add l) ++ map (Branch x l) (add r)

cbalTrees' :: Int -> [Tree Char]
cbalTrees' 0 = [Empty]
cbalTrees' n =
  let (q, r) = (n - 1) `quotRem` 2
   in [ Branch 'x' left right
        | i <- [q .. q + r],
          left <- cbalTrees' i,
          right <- cbalTrees' (n - i - 1)
      ]

-----------TREES--------------------------------------------------------
