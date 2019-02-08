import Text.Printf

main :: IO ()
main = do
        getLine
        args_ <- getLine
        let args = map read $ words args_
        let strArray = solve args
        print (strArray!!0)
        print (strArray!!1)
        print (strArray!!2)

solve :: [Int] -> [Float]
solve xs = map read (p : n : z : [])
    where (p,n,z) = ratios $ mkTriple xs (0,0,0)

roundTo6 f =  (fromInteger $ round $ f * (10^6)) / (10.0^^6)

roundToStr :: (PrintfArg a, Floating a) => Int -> a -> String
roundToStr = printf "%0.*f"

ratios :: (Float,Float,Float) -> (String,String,String)
ratios (p,n,z) = (roundToStr 6 $ p/total, roundToStr 6 $ n/total , roundToStr 6 $ z/total)
    where total = p + n + z

mkTriple :: [Int] -> (Float,Float,Float) -> (Float,Float,Float)
mkTriple [] a = a
mkTriple (x:xs) (p,n,z) | x > 0 = mkTriple xs (p + 1.0,n,z)
                        | x < 0 = mkTriple xs (p,n + 1.0,z)
                        | otherwise = mkTriple xs (p,n,z + 1.0)
