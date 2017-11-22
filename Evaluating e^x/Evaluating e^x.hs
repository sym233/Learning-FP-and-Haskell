module Main where

import Control.Monad
import Text.Printf as P

maxTerm = 10

factorial :: Int -> Int
factorial n
    | n <= 1 = 1
    | otherwise = n * factorial (n-1)

ex :: Double -> Int -> Double
ex x term
    | term >= maxTerm = 0
    | otherwise = x ^ term / (fromIntegral $ factorial term) + ex x (term+1)

main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    forM_ [1..n] $ \a0  -> do
        x_temp <- getLine
        let x = read x_temp :: Double
        P.printf "%.4f\n" (ex x 0)
