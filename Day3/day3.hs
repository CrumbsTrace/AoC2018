module Main where
import qualified Data.Map as Map
import Data.List
main = do
    p1
    p2 

p1 = do 
    xs <- map parseInput . lines <$> readFile "./day3.txt"
    print $ length $ Map.filter (>1) $ updateMap xs $ Map.fromList [((x, y), 0) | x <- [0..1000], y <- [0 .. 1000]]

p2 = do
    xs <- map parseInput . lines <$> readFile "./day3.txt"
    print $ getNonOverlapping xs 

getNonOverlapping :: [(Int, (Int, Int), (Int, Int))] -> Int 
getNonOverlapping (x@(id, min_c, max_c):xs) = if overlapping x xs then getNonOverlapping (xs ++ [x]) else id 

overlapping :: (Int, (Int, Int), (Int, Int)) -> [(Int, (Int, Int), (Int, Int))] -> Bool 
overlapping tile = any (overlap tile)
    where overlap (_, (min_x, min_y), (max_x, max_y)) (_, (n_min_x, n_min_y), (n_max_x, n_max_y))
                = not (min_x >= n_max_x || n_min_x >= max_x) && not (min_y >= n_max_y || n_min_y >= max_y)

updateMap :: [(Int, (Int, Int), (Int, Int))] -> Map.Map (Int, Int) Int -> Map.Map (Int, Int) Int
updateMap xs occ_map = new_map
    where   new_map = foldl' (foldl' (\ acc' (x, y) -> Map.adjust (+1) (x, y) acc')) occ_map coords
            coords = foldl' (\acc (_, (min_x, min_y), (max_x, max_y))-> [(x, y) | x <- [min_x .. max_x -1], y <- [min_y .. max_y -1]]:acc) [] xs

relativeToAbsolutePosition :: [Int] -> (Int, (Int, Int), (Int, Int))
relativeToAbsolutePosition [elf, x, y, dx, dy] = (elf, (x, y), (dx + x, dy + y))

parseInput :: [Char] -> (Int, (Int, Int), (Int, Int))
parseInput xs = relativeToAbsolutePosition $ map read $ splitOn (`elem` ['#', ' ', '@', ':', 'x' , ',']) xs

splitOn     :: (Char -> Bool) -> String -> [String]
splitOn p s =  case dropWhile p s of
                    "" -> []
                    s' -> w : splitOn p s''
                        where (w, s'') = break p s'
