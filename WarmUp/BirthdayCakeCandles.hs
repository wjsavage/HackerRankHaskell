solve :: [Int] -> String
solve xs = show . length . filter (== maximum xs) $ xs

main :: IO ()
main = interact $ solve . concat . map (map read . words) . tail . lines
