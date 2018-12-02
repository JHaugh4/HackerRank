module ThirtyDays where

import Data.List

--Day 11
testIn11 :: [[Integer]]
testIn11 = [
  [1,1,1,0,0,0],
  [0,1,0,0,0,0],
  [1,1,1,0,0,0],
  [0,0,2,4,4,0],
  [0,0,0,2,0,0],
  [0,0,1,2,4,0]]

hourglassSum :: [[Integer]] -> [Integer]
hourglassSum xss
  | length xss < 3   = []
  | otherwise        = 
    (hourglass (xss !! 0) (xss !! 1) (xss !! 2)) ++ hourglassSum (tail xss)
  where
    hourglass :: [Integer] -> [Integer] -> [Integer] -> [Integer]
    hourglass xs ys zs
      | length xs < 3 = []
      | otherwise     =
        let
          x = sum $ take 3 xs
          y = head . tail $ ys
          z = sum $ take 3 zs
        in (x+y+z) : hourglass (tail xs) (tail ys) (tail zs)

main :: IO ()
main = interact $ 
  show . maximum . hourglassSum . (fmap . fmap) read . fmap words . lines