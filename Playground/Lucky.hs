module Playground.Lucky (
  lucky,
) where

lucky:: (Integral a) => a -> String
lucky 7 = "LUCK NUMBER SEVEN!!"
lucky x = "Sorry, you are out of luck, pal!"