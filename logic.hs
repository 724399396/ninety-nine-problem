import Control.Monad
import Data.List
import Data.Ord (comparing)

not' :: Bool -> Bool
not' True = False
not' _ = True

and', or', nand', nor', xor', impl', equ' :: Bool -> Bool -> Bool

and' True True = True
and' _ _ = False

or' False False = False
or' _ _ = True

nand' a b = not' $ and' a b
nor' a b = not' $ or' a b

xor' a b
  | a == b = False
  | otherwise = True

impl' a b = (not' a) `or'` b

equ' a b = not $ xor' a b

table :: (Bool -> Bool -> Bool) -> IO ()
table f =
  mapM_ (putStrLn . foldl (\acc x -> acc++" "++x) "" . map show) [[a,b,r]| a <- [True,False], b <- [True,False], let r = f a b]

table2 = table

infixl 4 `or'`
infixl 4 `nor'`
infixl 5 `xor'`
infixl 6 `and'`
infixl 6 `nand'`
infixl 3 `equ'`

tablen n f = mapM_ putStrLn [toStr a ++ " => " ++ show (f a) | a <- args n]
  where args n = replicateM n [True, False]
        toStr = unwords . map (\x -> show x ++ space x)
        space True = "  "
        space False = " "

gray 1 = ["0", "1"]
gray n = let left = gray (n-1)
  in map ('0':) left ++ map ('1':) (reverse left)

data HTree a = Leaf a | Branch (HTree a) (HTree a)
  deriving Show

huffman :: (Ord a, Ord w, Num w) => [(a,w)] -> [(a,[Char])]
huffman freq = sortBy (comparing fst) $ serialize $
        htree $ sortBy (comparing fst) $ [(w, Leaf x) | (x,w) <- freq]
  where htree [(_, t)] = t
        htree ((w1,t1):(w2,t2):wts) =
          htree $ insertBy (comparing fst) (w1 + w2, Branch t1 t2) wts
        serialize (Branch l r) =
          [(x, '0':code) | (x, code) <- serialize l] ++
          [(x, '1':code) | (x, code) <- serialize r]
        serialize (Leaf x) = [(x, "")]
