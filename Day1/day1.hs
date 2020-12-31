{-# LANGUAGE TypeApplications #-}
module Main where
import qualified Data.Set as Set
main = do
    p1
    p2

p1 = do 
    xs <- parseInput
    print $ sum xs

p2 = do 
    xs <- cycle <$> parseInput
    print $ head $ findDuplicates Set.empty $ cumulativeSum 0 xs

findDuplicates :: Set.Set Int -> [Int] -> [Int]
findDuplicates set (x:xs) = if Set.member x set then x:findDuplicates newSet xs else findDuplicates newSet xs 
                            where newSet = Set.insert x set

cumulativeSum :: Int -> [Int] -> [Int]
cumulativeSum current (x:xs) = (current + x):cumulativeSum (current + x) xs 

parseInput :: IO [Int]
parseInput = map (read . removeC '+' ) . lines <$> readFile "./day1.txt"

removeC :: Char -> [Char] -> [Char]
removeC _  [] = []
removeC c (x:xs) = if x == c then removeC c xs else x:removeC c xs
