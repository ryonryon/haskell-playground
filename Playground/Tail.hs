module Playground.Tail (
  tail'
) where

tail':: [a] -> a
tail' [] = error "Can't call tail on an empty list, dummy!"
tail' ()