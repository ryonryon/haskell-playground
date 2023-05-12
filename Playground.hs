module Playground (
  add,
  bmiTell,
  capital,
  head',
  length',
  lucky,
  tail',
  tell,
) where

add:: Int -> Int -> Int
add x y = x + y 

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= skinny = "You're underweight, you emo, you!"
  | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= fat = "You're fat! Lose some weight, fatty!"
  | otherwise   = "You're a whale, congratulations!"
  where bmi = weight / height ^ 2
        skinny = 18.5
        normal = 25.0
        fat = 30.0

capital:: String -> String
capital [] = "Empty string, Whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

head':: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

length':: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

lucky:: (Integral a) => a -> String
lucky 7 = "LUCK NUMBER SEVEN!!"
lucky x = "Sorry, you are out of luck, pal!"

tail':: [a] -> a
tail' [] = error "Can't call tail on an empty list, dummy!"
tail' ()

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "the list have two element: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "the list is long. The first tow elements are: " ++ show x ++ " and " ++ show y