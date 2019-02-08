main :: IO ()
main = interact $ show . sum . concat . map (map read . words) . tail . lines
