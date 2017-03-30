import Control.Monad

data Tree a = Empty | Branch a (Tree a) (Tree a)
                  deriving (Show, Eq)

leaf x = Branch x Empty Empty

treeLength :: Tree a -> Int
treeLength Empty = 0
treeLength (Branch _ l r) = 1 + treeLength l + treeLength r

cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n = let (q,r) = (n-1) `quotRem` 2
             in [Branch 'x' left right| i <- [q..(q+r)],
                                         left <- cbalTree i,
                                         right <- cbalTree (n-1-i)]

symmetric Empty = True
symmetric (Branch _ l r) = mirror l r
  where mirror Empty Empty = True
        mirror (Branch _ l1 r1) (Branch _ l2 r2) = mirror l1 r2 && mirror r1 l2
        mirror _ _ = False

add :: Ord a => Tree a -> a -> Tree a
add Empty node = leaf node
add (Branch x l r) node = if (node < x)
                          then Branch x (add l node) r
                          else Branch x l (add r node)
                     
construct :: Ord a => [a] -> Tree a
construct = foldl add Empty

symCbalTrees' = filter symmetric . cbalTree

symCbalTrees n = if n `mod` 2 == 0
                 then []
                 else [Branch 'x' t (reverseTree t) | t <- cbalTree (n `div` 2)]

reverseTree Empty = Empty
reverseTree (Branch x l r) = Branch x (reverseTree r) (reverseTree l)

hbalTree x 0 = [Empty]
hbalTree x 1 = [Branch x Empty Empty]
hbalTree x h = do (lh,rh) <- [(h-2,h-1), (h-1,h-1), (h-1,h-2)]
                  lt <- hbalTree x lh
                  rt <- hbalTree x rh
                  return $ Branch x lt rt

maxNodes :: Int -> Int
maxNodes h = 2^h - 1

minHeight :: Int -> Int
minHeight n = ceiling $ logBase 2 $ fromIntegral (n+1)

minNodes :: Int -> Int
minNodes h = fibs !! (h+2) - 1

maxHeight :: Int -> Int
maxHeight n = length (takeWhile (<= n+1) fibs) - 3

fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

hbalTreeNodes :: a -> Int -> [Tree a]
hbalTreeNodes x n = [t | h <- [minHeight n .. maxHeight n], t <- baltree h n]
  where
    -- baltree h n = weighte-balanced trees of height h with n nodes
    -- assuming minNodes h <= n <= maxNodes h
    baltree 0 n = [Empty]
    baltree 1 n = [Branch x Empty Empty]
    baltree h n = [Branch x l r |
                   (hl, hr) <- [(h-2,h-1), (h-1,h-1), (h-1,h-2)],
                   let min_nl = max (minNodes hl) (n - 1 - maxNodes hr),
                   let max_nl = min (maxNodes hl) (n - 1 - minNodes hr),
                   nl <- [min_nl .. max_nl],
                   let nr = n - 1 - nl,
                   l <- baltree hl nl,
                   r <- baltree hr nr]
