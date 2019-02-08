compare' :: [Int] -> [Int] -> (Int, Int)
compare' [] [] = (0,0)
compare' (a:as) (b:bs) | a < b = (fst next, snd next + 1)
                      | a > b = (fst next + 1, snd next)
                      | otherwise = next
                        where next  = compare' as bs

solve :: [[Int]] -> String
solve (as : bs : []) = (show $ fst results) ++ " " ++ (show $ snd results)
    where results = compare' as bs

main :: IO ()
main = interact $ solve . map (map read . words) . lines
