module Playground (
  add,
  applyTwice,
  bmiTell,
  calcBmis,
  capital,
  describeList,
  elem',
  head',
  lucky,
  maximum',
  replicate',
  reverse',
  tail',
  take',
  tell,
  zip',
  zipWIth',
) where

add:: Int -> Int -> Int
add x y = x + y 

applyTwice:: (a -> a) -> a -> a
applyTwice f x = f (f x)

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= skinny = "You're underweight, you emo, you!"
  | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= fat = "You're fat! Lose some weight, fatty!"
  | otherwise   = "You're a whale, congratulations!"
  where bmi = weight / height ^ 2
        (skinny, normal, fat) = (18.5, 25.0, 30.0)

calcBmis:: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs, let bmi w h = w / h ^ 2]

capital:: String -> String
capital [] = "Empty string, Whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

cylinder:: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in sideArea + 2 * topArea

describeList:: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."

elem':: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)
  | a == x = True
  | otherwise = elem' a xs

head':: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

initials:: String -> String -> String
initials firstName lastName = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstName
        (l:_) = lastName

lucky:: (Integral a) => a -> String
lucky 7 = "LUCK NUMBER SEVEN!!"
lucky x = "Sorry, you are out of luck, pal!"

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list!"
maximum' [x] = x
maximum' (x:xs)
  | x > maxTail = x
  | otherwise = maxTail
  where maxTail = maximum' xs

replicate':: (Num i, Ord i) => i -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x:replicate' (n - 1) x

reverse':: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

tail':: [a] -> a
tail' [] = error "Can't call tail on an empty list, dummy!"
tail' [x] = x
tail' (_:xs) = tail' xs

take':: (Num i, Ord i) => i -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x:take' (n - 1) xs

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell [x] = "The list has one element: " ++ show x
tell [x, y] = "the list have two element: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "the list is long. The first tow elements are: " ++ show x ++ " and " ++ show y

zip':: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

zipWith':: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWIth' f (x:xs) (y:ys) = f x y : zipWith' f xs ys