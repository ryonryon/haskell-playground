module Playground.Capital (
  capital
) where

capital:: String -> String
capital [] = "Empty string, Whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]