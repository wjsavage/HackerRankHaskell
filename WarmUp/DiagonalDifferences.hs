diagonal :: [[Int]] -> Int -> [Int]
diagonal [] _ = []
diagonal (xs:xss) n = xs!!n : diagonal xss (n + 1)

solve xss = abs $ sum (diagonal xss 0) - sum (diagonal (reverse xss) 0)

main :: IO ()
main = interact $ show . solve . map (map read . words) . tail . lines
