import Control.Monad

memoized_penta :: Int -> Int
memoized_penta = (map penta [0..] !!) where
  penta 1 = 1
  penta 2 = 5
  penta n = memoized_penta (n-1) + n + (n-1)*4 - 2 * (n-1)

memoized_penta' :: Int -> Int
memoized_penta' = (map penta [0..] !!) where
  penta 1 = 1
  penta 2 = 5
  penta n = 2 * n - 6 + memoized_penta (n-1) 

penta' :: Int -> Int
penta' 1 = 1
penta' 2 = 5
penta' n = 2 * n - 6 + penta' (n-1) 

data Tree a = Tree (Tree a) a (Tree a)

instance Functor Tree where
    fmap f (Tree l m r) = Tree (fmap f l) (f m) (fmap f r)

f :: (Int -> Int) -> Int -> Int
f mf 1 = 1
f mf 2 = 5
f mf n = 3 * n - 2 + mf (n-1) 

index :: Tree a -> Int -> a
index (Tree _ m _) 0 = m
index (Tree l _ r) n = case (n - 1) `divMod` 2 of
    (q,0) -> index l q
    (q,1) -> index r q

nats :: Tree Int
nats = go 0 1 where
  go n s = Tree (go l s') n (go r s') where
    l = n + s
    r = l + s
    s' = s * 2

toList :: Tree a -> [a]
toList as = map (index as) [0..]

f_tree :: Tree Int
f_tree = fmap (f fastest_f) nats

fastest_f :: Int -> Int
fastest_f = index f_tree

main = do
    --n <- readLn
    --xs <- replicateM n readLn
    --mapM_ print (map fastest_f xs)
    print $ fastest_f 100000
    print $ fastest_f 100001