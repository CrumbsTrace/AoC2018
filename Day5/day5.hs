{-# LANGUAGE TypeApplications #-}
module Main where

import qualified Data.Char as Char
import qualified Data.List as List

main = do 
    xs <- head . lines <$> readFile "./day5.txt"
    putStr "p1: "
    print $ p1 xs
    putStr "p2: "
    print $ p2 xs


p1 :: String -> Int
p1 = length . causeReactions

p2 :: [Char] -> Int
p2 str = minimum lengths
    where
        str'                  = causeReactions str
        uniqueLetters         = List.nub (map Char.toUpper str')
        lengths               = [p1 (filter (\x -> Char.toUpper x /= toRemove) str') | toRemove <- uniqueLetters]

causeReactions :: String -> String
causeReactions = foldr step ""
    where
    step x (y:ys) | abs (Char.ord x - Char.ord y) == 32 = ys
    step x ys                                           = x : ys