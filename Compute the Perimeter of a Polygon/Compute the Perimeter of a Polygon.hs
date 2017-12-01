-- Enter your code here. Read input from STDIN. Print output to STDOUT
import Control.Monad (replicateM)

perimeter :: Int -> [(Int, Int)] -> Double
perimeter n co = sum $ map (\i-> sqrt $ fromIntegral ((dx i)^2 + (dy i)^2)) [0.. (n-1)] where
    dx i = (fst (co!!((i+1) `rem` n))) - fst (co!!i)
    dy i = (snd (co!!((i+1) `rem` n))) - snd (co!!i)
    

main :: IO()
main = do
    strN <- getLine
    let n = read strN :: Int
    strCo <- replicateM n getLine

    let strXY = map (\str->words str) strCo
    let coordinates = map (\strXY->((read (strXY!!0) :: Int), (read (strXY!!1) :: Int))) strXY
    print (perimeter n coordinates)
