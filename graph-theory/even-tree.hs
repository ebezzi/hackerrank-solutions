import Data.List
import Data.Maybe

getMultipleLines :: Int -> IO [String]
getMultipleLines n
    | n <= 0 = return []
    | otherwise = do          
        x <- getLine         
        xs <- getMultipleLines (n-1)    
        return (x:xs)

toInt :: String -> Int
toInt xs = read xs :: Int

type Point  = (Int, Int)
type Edge   = Point
type Vertex = Int 

point :: String -> Point
point xs = (toInt x, toInt y) where
    x:y:[] = words xs

data Tree = Leaf Int | Node Int [Tree] deriving (Eq, Show)

vertices :: [Edge] -> [Vertex]
vertices xs = nub (map fst xs ++ map snd xs)

isLeaf :: [Edge] -> Vertex -> Bool
isLeaf es v = isNothing $ find (\x -> v == snd x) es

descendants :: [Edge] -> Vertex -> [Vertex]
descendants es v = map fst $ filter (\x -> v == snd x) es

mkTree :: [Edge] -> Vertex -> Tree
mkTree es v
    | isLeaf es v = Leaf v
    | otherwise = Node v (map (mkTree es) (descendants es v))

main :: IO ()
main = do
    first <- getLine
    let (n,m) = point first
    edgesTemp <- getMultipleLines m
    let edges = map point edgesTemp
    let tree = mkTree edges 1
    --print tree
