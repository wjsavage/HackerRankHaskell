solve (x:y:[]) = sum y

main :: IO ()
main = interact $ show . solve . map (map read . words) . lines
