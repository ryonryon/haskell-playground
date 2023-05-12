module Math (
  abs',
  min',
  max',
  sum',
) where

abs':: (Integral a) => a -> a
abs' a
  | a < 0 = -1 * a
  | otherwise = a

min':: (Integral a) => a -> a -> a
min' a b
  | a < b = a
  | otherwise = b

max':: (Integral a) => a -> a -> a
max' a b
  | a < b = b
  | otherwise = a

sum':: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs