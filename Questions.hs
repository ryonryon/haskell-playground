module Questions (
  findLast,
  findSecondLast,
  findK,
  length',
  isPalindrome,
  flatten',
  compress',
  pack',
) where
-- https://wiki.haskell.org/99_questions
import Data.List

-- 1. (*) Find the last element of a list.
findLast:: [a] -> a
findLast [] = error "The list is empty"
findLast [x] = x
findLast (_:xs) = findLast xs

-- 2. (*) Find the last but one element of a list.
findSecondLast:: [a] -> a
findSecondLast [] = error "The list is empty"
findSecondLast [a] = error "The list does not have enough element"
findSecondLast (x:xs) =
  if length xs == 1 then x
  else findSecondLast xs

-- 3. (*) Find the K'th element of a list. The first element in the list is number 1.
findK:: [a] -> Int -> a
findK [] _ = error "Index out of bounds"
findK (x:_) 1 = x
findK (_:xs) k
  | k < 1 = error "Index out of bounds"
  | otherwise = findK xs (k - 1)

-- 4. (*) Find the number of elements of a list.
length':: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

-- 5. (*) Reverse a list.
reverse':: [a] -> [a]
reverse' [] = []
reverse' [a] = [a]
reverse' (x:xs) = reverse' xs ++ [x]

-- 6. (*) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
isPalindrome:: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome xs = head' xs == last' xs && isPalindrome (init $ tail xs)

head':: [a] -> a
head' [] = error "The list is empty"
head' (x:_) = x

last':: [a] -> a
last' [] = error "The list is empty"
last' [x] = x
last' (_:xs) = last' xs

-- 7. (**) Flatten a nested list structure.
-- Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).
data NestedList a = Elem a | List [NestedList a]
flatten':: NestedList a -> [a]
flatten' (Elem x) = [x]
flatten' (List (x:xs)) = flatten' x ++ flatten' (List xs)
flatten' (List []) = []

-- 8. (**) Eliminate consecutive duplicates of list elements.
-- If a list contains repeated elements they should be replaced with a single copy of the element.
-- The order of the elements should not be changed.
compress':: (Eq a) => [a] -> [a]
compress' (x:ys@(y:_))
    | x == y    = compress' ys
    | otherwise = x : compress' ys
compress' ys = ys
-- compress' = map head . group

-- 9. (**) Pack consecutive duplicates of list elements into sub-lists. 
-- If a list contains repeated elements they should be placed in separate sub-lists.
pack':: Eq a => [a] -> [[a]]
pack' [] = []
pack' (x:xs) = (x:first) : pack' rest
  where
    getReps [] = ([], [])
    getReps (y:ys)
      | y == x = let (f, r) = getReps ys in (y:f, r)
      | otherwise = ([], y:ys)
    (first, rest) = getReps xs

-- 10. (*) Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method.
-- Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
