module Day1 where

import Data.Bifunctor (Bifunctor (bimap))
import Data.List (sort)

parse :: String -> ([Int], [Int])
parse str = foldl (\(xs, ys) (x, y) -> (x : xs, y : ys)) ([], []) pairs
  where
    pairs = map ((\case [x, y] -> (read x, read y); _ -> (0, 0)) . filter (/= "|") . words) (lines str)

p1 :: ([Int], [Int]) -> Int
p1 input = sum $ uncurry (zipWith (\x y -> abs (x - y))) $ bimap sort sort input

p2 :: ([Int], [Int]) -> Int
p2 input = sum [x * foldl (\a y -> if x == y then a + 1 else a) 0 (snd input) | x <- fst input]
