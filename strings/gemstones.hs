import Data.List

getMultipleLines :: Int -> IO [String]
getMultipleLines n
    | n <= 0 = return []
    | otherwise = do          
        x <- getLine         
        xs <- getMultipleLines (n-1)    
        return (x:xs)            

main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    xs <- getMultipleLines n
    let res = foldl intersect (head xs) xs
    print $ (length . nub) res