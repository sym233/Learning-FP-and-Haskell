module Main where

f :: [Int] -> [Int]
f (x1: x2: xs) = x2: f xs
f _ = []

main :: IO()
main = do
   inputdata <- getContents
   mapM_ (putStrLn. show). f. map read. lines $ inputdata
