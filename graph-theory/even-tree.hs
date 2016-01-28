import Data.List

getMultipleLines :: Int -> IO [String]
getMultipleLines n
    | n <= 0 = return []
    | otherwise = do          
        x <- getLine         
        xs <- getMultipleLines (n-1)    
        return (x:xs)

parseInt :: IO String
parseInt = do
    
toInt :: String -> Int
toInt xs = read xs :: Int

tuple :: String -> (Int, Int)
tuple xs = (toInt x, toInt y) where
    x:y:[] = words xs

main :: IO ()
main = do
    first <- getLine
    let (n,m) = tuple first
    edgesTemp <- getMultipleLines m
    let edges = map tuple edgesTemp
    print edges
