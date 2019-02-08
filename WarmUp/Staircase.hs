main :: IO()
main = do
    n <- getLine
    let size = read n
    let strArray = staircase (size-1) 1
    mapM_ putStrLn strArray

staircase :: Int -> Int -> [String]
staircase 0 hash = replicate hash '#' : []
staircase spaces hash = (replicate spaces ' ' ++ replicate hash '#') : staircase (spaces - 1) (hash + 1)
