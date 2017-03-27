import Data.List
import System.Random (getStdGen, randomRs, randomRIO)

myLast :: [a] -> a
myLast [] = error "No end for empty lists!"
myLast [x] = x
myLast (_:xs) = myLast xs

myButLast :: [a] -> a
myButLast [] = error "empty list!"
myButLast [_] = error "only one element list!"
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs

elementAt :: [a] -> Int -> a
elementAt [] _ = error "empty list!"
elementAt (x:xs) i
  | i < 1 = error "error index"
  | i == 1 = x
  | otherwise = elementAt xs (i - 1)

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + (myLength xs)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = x == (reverse x)

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = concat $ map flatten xs

compress :: Eq a => [a] -> [a]
compress = map head . group
          
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = let (first,rest) = span (== x) xs
              in (x:first): (pack rest)

encode :: Eq a => [a] -> [(Int,a)]
encode = map (\x -> (length x, head x)) . pack

data WordNumber a = Single a | Multiple Int a deriving (Show)
encodeModified :: Eq a => [a] -> [WordNumber a]
encodeModified = map encoderHelper . encode
  where
    encoderHelper (1,x) = Single x
    encoderHelper (n,x) = Multiple n x

decodeModified :: [WordNumber a] -> [a]
decodeModified = concatMap decodeHelper
  where decodeHelper (Single x) = [x]
        decodeHelper(Multiple n x) = replicate n x

encode' :: Eq a => [a] -> [(Int,a)]
encode' = foldr helper []
  where
    helper x [] = [(1,x)]
    helper x (y@(a,b):ys)
      | x == b = ((1+a,b):ys)
      | otherwise = (1,x):y:ys

encodeDirect :: Eq a => [a] -> [WordNumber a]
encodeDirect = map encoderHelper . encode'
  where
    encoderHelper (1,x) = Single x
    encoderHelper (n,x) = Multiple n x

dupli :: [a] -> [a]
dupli = concat . map (replicate 2)

repli :: [a] -> Int -> [a]
repli x n = concat $ map (replicate n) x

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = map fst $ filter (\(_,i) -> i `mod` n /= 0) $ zip xs [1..]

split :: [a] -> Int -> ([a],[a])
split xs 0 = ([], xs)
split [] _ = ([],[])
split (x:xs) n = case split xs (n-1) of
  (f,r) -> (x:f,r)

slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice (x:xs) l r
  | l > r = []
  | l > 1 = slice xs (l-1) (r-1)
  | r < 1 = []
  | otherwise = x:(slice xs (l-1) (r-1))

rotate :: [a] -> Int -> [a]
rotate x n
  | n < 0 = rotate ((last x) : (init x)) (n+1)
  | n > 0 = rotate ((tail x) ++ [(head x)]) (n-1)
  | otherwise = x
      
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs =  (helper1 xs n, helper2 xs n)
  where
    helper1 (x:_) 1 = x
    helper1 (_:xs) n = helper1 xs (n-1)
    helper2 (_:xs) 1 = xs
    helper2 (x:xs) n = x : (helper2 xs (n-1))

--21 
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = let (h,t) = splitAt (n-1) xs
                  in
                    (h ++ (x:t))

-- 22
range :: Integral a => a -> a -> [a]
range low high
  | low > high = []
  | otherwise = low : range (low+1) high

-- 23
rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = do
  let l = length xs
  stdGen <- getStdGen
  return $ map (xs!!) $ take n $ randomRs (1,l) stdGen



--24
rnd_select_from_list :: Eq a => Int -> [a] -> IO [a]
rnd_select_from_list 0 _ = return []
rnd_select_from_list n xs = do
          loc <- randomRIO (0, length xs - 1)
          let x = xs !! loc
          left <- rnd_select_from_list (n-1) (delete x xs)
          return $ x:left


diff_select :: Int -> Int -> IO [Int]
diff_select n high =
  rnd_select_from_list n [1..high]
         
--25
rnd_permu :: Eq a => [a] -> IO [a]
rnd_permu xs = rnd_select_from_list (length xs) xs

--26
combinations :: Ord a => Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = nub $ map sort $ do
  x <- xs
  left <- combinations (n-1) (delete x xs)
  return $ x:left

--27
my_group :: Ord a => [Int] -> [a] -> [[[a]]]
my_group [] _ = [[[]]]
my_group ns xs = nub $ map sort $ do
  n <- ns
  hs <- combinations n xs
  left <- my_group (tail ns) (xs \\ hs)
  return $ map (hs++) left

--28
lsort :: [[a]] -> [[a]]
lsort = sortOn length

lfsort :: Ord a => [[a]] -> [[a]]
lfsort xs = reverse $ sortOn freLoc xs
  where
    freqs = sort $ map length xs
    freqLenthSort = map head $ sortOn length $ group freqs
    freLoc x = maybe (-1) id (find (==(length x)) freqLenthSort)    
