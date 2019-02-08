compare' :: [Int] -> (Int, Int) -> (Int,Int)
compare' [] y = y
compare' (x:xs) (a,b) | x < 0 = (fst next, snd next + 1)
                      | x > 0 = (fst next + 1, snd next)
                      | otherwise = next
                        where next  = compare' xs (a,b)

solve :: [[Int]] -> String
solve (as : bs : []) = (show $ fst results) ++ " " ++ (show $ snd results)
    where results = compare' (zipWith (-) as bs) (0,0)

main :: IO ()
main = interact $ solve . map (map read . words) . lines
