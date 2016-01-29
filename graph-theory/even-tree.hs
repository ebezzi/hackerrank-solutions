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
descendants es v = map fst $ edges es v

edges :: [Edge] -> Vertex -> [Edge]
edges es v = filter (\x -> v == snd x) es

countEdges :: Tree -> Int
countEdges (Leaf _) = 1
countEdges (Node _ xs) = length xs + map countEdges xs

mkTree :: [Edge] -> Vertex -> Tree
mkTree es v
    | isLeaf es v = Leaf v
    | otherwise = Node v (map (mkTree es) (descendants es v))

dropEdges :: Tree -> Int
dropEdges (Leaf x) = 0
dropEdges (Node x xs) = length evenSubtrees + map dropEdges oddSubtrees where 
    evenSubtrees = filter (\x -> even (countEdges x)) subtrees
    oddSubtrees = filter (\x -> odd (countEdges x)) subtrees
    subtrees = xs

main :: IO ()
main = do
    first <- getLine
    let (n,m) = point first
    edgesTemp <- getMultipleLines m
    let edges = map point edgesTemp
    let tree = mkTree edges 3
    print tree



