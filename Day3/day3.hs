module Main where
import qualified Data.Set as Set
import Data.List
type BoundsClaim = (Int, (Int, Int), (Int, Int))

main = do
    p1
    p2 

p1 = do 
    xs <- map parseInput . lines <$> readFile "./day3.txt"
    print $ length $ getAllOverlapSquares xs

p2 = do
    xs <- map parseInput . lines <$> readFile "./day3.txt"
    print $ getNonOverlapping xs 

getAllOverlapSquares :: [BoundsClaim] -> Set.Set (Int, Int)
getAllOverlapSquares xs = foldr (\(c1, c2) acc -> if overlap c1 c2 then Set.union acc $ getOverlapRectangle c1 c2 else acc) Set.empty claimPairs
    where claimPairs = [(x, y) | (x:ys) <- tails xs, y <- ys]

getNonOverlapping :: [BoundsClaim] -> Int 
getNonOverlapping (x@(id, min_c, max_c):xs) = if overlapping x xs then getNonOverlapping (xs ++ [x]) else id 

overlapping :: BoundsClaim -> [BoundsClaim] -> Bool 
overlapping tile = any (overlap tile)
    where overlap (_, (min_x, min_y), (max_x, max_y)) (_, (n_min_x, n_min_y), (n_max_x, n_max_y))
                = not (min_x >= n_max_x || n_min_x >= max_x) && not (min_y >= n_max_y || n_min_y >= max_y)

overlap :: BoundsClaim -> BoundsClaim -> Bool
overlap (_, (min_x, min_y), (max_x, max_y)) (_, (n_min_x, n_min_y), (n_max_x, n_max_y))
                = not (min_x >= n_max_x || n_min_x >= max_x) && not (min_y >= n_max_y || n_min_y >= max_y)

getOverlapRectangle :: BoundsClaim -> BoundsClaim -> Set.Set (Int, Int)
getOverlapRectangle (_, (min_x, min_y), (max_x, max_y)) (_, (min_x2, min_y2), (max_x2, max_y2)) = 
    Set.fromList $ [(x, y) | x <- [o_x_min..o_x_max - 1], y <- [o_y_min..o_y_max - 1]]
    where   o_x_max = min max_x max_x2
            o_y_max = min max_y max_y2
            o_x_min = max min_x min_x2
            o_y_min = max min_y min_y2

relativeToAbsolutePosition :: [Int] -> BoundsClaim
relativeToAbsolutePosition [elf, x, y, dx, dy] = (elf, (x, y), (dx + x, dy + y))

parseInput :: [Char] -> BoundsClaim
parseInput xs = relativeToAbsolutePosition $ map read $ splitOn (`elem` ['#', ' ', '@', ':', 'x' , ',']) xs

splitOn     :: (Char -> Bool) -> String -> [String]
splitOn p s =  case dropWhile p s of
                    "" -> []
                    s' -> w : splitOn p s''
                        where (w, s'') = break p s'
