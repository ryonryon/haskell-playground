module Leetcode.TwoSum (
  twoSum,
) where
-- TODO: fix me
twoSum:: [Int] -> Int -> [Int]
twoSum [] _ = []
twoSum [x] a
  | x == a = [x]
  | otherwise = []
twoSum (x:sx) a
  | x == a = [x]
  | x < a = x: twoSum sx (a - x)
  | otherwise = []