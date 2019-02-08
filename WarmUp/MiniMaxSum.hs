import Data.List

solve :: [Int] -> String
solve xs = show b ++ " " ++ show a
  where a = sum $ delete (minimum xs) xs
        b = sum $ delete (maximum xs) xs

main :: IO ()
main = interact $ solve . concat . map (map read . words) . lines
