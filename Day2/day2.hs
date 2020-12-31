{-# LANGUAGE TupleSections #-}
module Day2 where
 
import Data.List
main = do
  p1


p1 = do 
    xs <- map sort . lines <$> readFile "day2.txt"
    let doubles = filter (hasNRepeatingChar 2) xs
    let triples = filter (hasNRepeatingChar 3) xs
    print $ length doubles * length triples

hasNRepeatingChar :: Int -> [Char] -> Bool 
hasNRepeatingChar n xs = any (\x -> length x == n) (group xs)

p2 = do
    xs <- lines <$> readFile "day2.txt"
    let combinations = [(x, y) | (x:ys) <- tails xs, y <- ys ]
    let correctBoxes = filter (\(_, x) -> x == 1) $ map (\x -> (x, uncurry differenceCount x)) combinations
    print $ uncurry removeDifference <$> map fst correctBoxes

removeDifference :: [Char] -> [Char] -> [Char]
removeDifference [] [] = []
removeDifference (x:xs) (y:ys) = if x == y then x:removeDifference xs ys else removeDifference xs ys

differenceCount :: [Char] -> [Char] -> Int 
differenceCount [] [] = 0
differenceCount (x:xs) (y:ys) = if x /= y then 1 + differenceCount xs ys else differenceCount xs ys

test = print "hellpro"
