{-# LANGUAGE TypeApplications #-}
module Main where

import Data.List
import qualified Data.Map as Map
import qualified Data.Bifunctor as Bifunctor 

type MinuteRange = (Int, Int)
type DateTime = (Int, Int, Int, Int, Int)
type Schedule = (DateTime, String) 
type GuardSleepTimes = (Int, [MinuteRange])
main = do p1p2

p1p2 = do 
    xs <- sort . map (parseDateSection . splitOn (`elem` ['[', ']']))  . lines <$> readFile "./day4.txt"
    let sleepTimes = map toGuardSleepTIme $ groupGuards xs []
    let guardSleepTimes = map combineSleepTimes $ (groupBy (\(id, _) (id2, _) -> id == id2) . sort) sleepTimes
    let mostAsleepGuard = maximumBy cumulativeSleepOrd guardSleepTimes
    let mostAsleepAtOneMinute = maximumBy (\x y -> minuteRangeEq (snd x) (snd y)) $ map (Bifunctor.second mostSleptMinute) guardSleepTimes

    putStr "Most sleeping guard: "
    print $ fst mostAsleepGuard * fst (mostSleptMinute (snd mostAsleepGuard))

    putStr "Most often asleep at 1 minute: "
    print $ fst mostAsleepAtOneMinute * fst (snd mostAsleepAtOneMinute)

mostSleptMinute :: [MinuteRange] -> (Int, Int)
mostSleptMinute xs = if null sleepMapResult then (-1, -1) else maximumBy minuteRangeEq sleepMapResult
    where   
        createSleepMap [] sleepMap = sleepMap
        createSleepMap ((t1, t2):ys) sleepMap = createSleepMap ys $ foldl (flip $ Map.alter (Just . maybe 1 (+1))) sleepMap [t1 .. t2 -1]  
        sleepMapResult = Map.toList $ createSleepMap xs Map.empty

minuteRangeEq :: MinuteRange -> MinuteRange -> Ordering
minuteRangeEq (_, a) (_, b)
            | a > b = GT
            | a < b = LT
            | a == b = EQ

cumulativeSleepOrd :: GuardSleepTimes -> GuardSleepTimes -> Ordering
cumulativeSleepOrd (_, x) (_, y)
    | amountSleptX > amountSleptY = GT
    | amountSleptX < amountSleptY = LT
    | amountSleptY == amountSleptX = EQ
    where 
        amountSleptX = totalSleep x
        amountSleptY = totalSleep y

totalSleep :: [MinuteRange] -> Int
totalSleep [] = 0
totalSleep ((b, e):xs) = e - b + totalSleep xs

combineSleepTimes:: [GuardSleepTimes] -> GuardSleepTimes
combineSleepTimes [] = (0, [])
combineSleepTimes ((id, t1):xs) = (id, t1 ++ t2)
    where (_, t2) = combineSleepTimes xs

toGuardSleepTIme:: [Schedule] -> GuardSleepTimes
toGuardSleepTIme ((_,guard):xs) = (guardId, sleepInMinutes xs)
    where guardId = read $ splitOn (`elem` [' ', '#']) guard !! 1

sleepInMinutes :: [Schedule] -> [MinuteRange]
sleepInMinutes [] = []
sleepInMinutes ((d1, s1):(d2, s2):xs) = minuteRange d2 d1:sleepInMinutes xs

minuteRange:: DateTime -> DateTime -> MinuteRange
minuteRange (y1, m1, d1, h1, mi1) (y2, m2, d2, h2, mi2) = (mi2, mi1)

groupGuards :: [Schedule] -> [Schedule] -> [[Schedule]]
groupGuards [] [] = []
groupGuards [] ys = reverse ys:groupGuards [] []
groupGuards (x@(_, action):xs) ys
    | "Guard" `isInfixOf` action = if ys /= [] then reverse ys:groupGuards xs [x] else groupGuards xs [x]
    | otherwise = groupGuards xs (x:ys)

parseDateSection :: [[Char]] -> Schedule
parseDateSection [dateTime, rest] =  (toDateTime $ map (read @Int) (splitOn (`elem` ['-', ' ', ':']) dateTime), rest)

toDateTime:: [Int]  -> DateTime
toDateTime [v1, v2, v3, v4, v5] = (v1, v2, v3, v4, v5)

splitOn     :: (Char -> Bool) -> String -> [String]
splitOn p s =  case dropWhile p s of
                    "" -> []
                    s' -> w : splitOn p s''
                        where (w, s'') = break p s'