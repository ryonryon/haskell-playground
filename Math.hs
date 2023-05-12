module Math (
  abs'
) where

abs':: (Integral a) => a -> a
abs' a
  | a < 0 = -1 * a
  | otherwise = a
