module Playground.Tell (
  tell
) where

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "the list have two element: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "the list is long. The first tow elements are: " ++ show x ++ " and " ++ show y