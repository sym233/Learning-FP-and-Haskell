module Main where

f :: Int -> [Int] -> [Int]
f n [] = []
f n arr = take n (repeat (head arr)) ++ f n (tail arr)

main :: IO ()
main = getContents >>=
       mapM_ print. (\(n:arr) -> f n arr). map read. words
