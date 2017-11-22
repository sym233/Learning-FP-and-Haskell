-- Enter your code here. Read input from STDIN. Print output to STDOUT

module Main where

import Control.Monad
import Data.List

factorial :: Int -> Int
factorial n
    | n <= 1 = 1
    | otherwise = n * (factorial (n - 1))

main :: IO()
main = do
    strK <- getLine
    let k = read strK :: Int
    forM_ [0.. k-1] (\n-> putStrLn $
        intercalate " " $
            map (\r-> 
                show $
                    div
                        (factorial n)
                        ((factorial r)*(factorial (n-r))))
                [0.. n])
