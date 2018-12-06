import Data.Map (Map)
import qualified Data.Map as M
import Cp (split, swap)
import Data.List (nub, group, sort)

type Coordinate = (Int, Int)
data Location = L (Int, Int) | C Int (Int, Int) | E (Int, Int)
              deriving (Show, Eq)
type Grid = [Location]

{-
    Advent of code - Day 6: Chronal Coordinates
    1st Part:
        What is the size of the largest area that isn't infinite?
-}

parse :: String -> Coordinate
parse s = read $ "(" ++ s ++ ")"

manhattan :: Coordinate -> Coordinate -> Int
manhattan (a,b) (c,d) = abs (a-c) + abs (b-d)

initialGrid :: [Coordinate] -> Grid
initialGrid cs = let csX = map fst cs
                     csY = map snd cs
                     minX = minimum csX
                     maxX = maximum csX
                     minY = minimum csY + 2
                     maxY = maximum csY - 5
                     in [L (x,y) | x <- [minX..maxX], y <- [minY..maxY]]

constructMap :: [Coordinate] -> Map Coordinate Int
constructMap l = go l 0 M.empty
    where
        go [] _ m = m
        go (x:xs) i m = go xs (i+1) (M.insert x i m) 

calcPosition1 :: [Coordinate] -> Map Coordinate Int -> Location -> Location
calcPosition1 cs m (L (a,b)) = findClosestCoord (a,b) cs m
    where
        findClosestCoord (a,b) cs m = let l           = map (split (manhattan (a,b)) (`M.lookup` m)) cs
                                          minD        = split length id . head . group . sort . map fst $ l
                                          hasRep      = (>1) . fst $ minD
                                          (Just i)    = snd . minimum $ l
                                          in if hasRep then E (a,b) else C i (a,b)

isFinite :: Coordinate -> [Coordinate] -> Bool
isFinite (a,b) cs = let h1 = any ((< a) . fst) cs
                        h2 = any ((> a) . fst) cs
                        v1 = any ((< b) . snd) cs
                        v2 = any ((> b) . snd) cs
                        in h1 && h2 && v1 && v2

part1 :: [Coordinate] -> Map Coordinate Int -> Grid -> Int
part1 cs m g = let finiteCoords = filter (`isFinite` cs) cs
                   areas = map (countArea m g) finiteCoords
                   in maximum areas
    where
        l (C i (a,b)) = i
        l _ = -1
        countArea m g c = let (Just i) = M.lookup c m
                              in length . filter ((== i) . l) $ g

main1 = do
        input <- lines <$> readFile "input.txt"
        let parsed    = map parse input
            m         = constructMap parsed
            finalGrid = map (calcPosition1 parsed m) (initialGrid parsed)
            res       = part1 parsed m finalGrid
        putStrLn . show $ res

{-
    2nd Part:
        What is the size of the region containing all locations which have a total distance to all given coordinates of less than 10000?
-}

calcPosition2 :: [Coordinate] -> Map Coordinate Int -> Location -> Location
calcPosition2 cs m (L (a,b)) = findClosestCoord (a,b) cs m
    where
        findClosestCoord (a,b) cs m = let l           = map (split (manhattan (a,b)) (`M.lookup` m)) cs
                                          distances   = map fst l
                                          isLess      = (<10000) . sum $ distances
                                          (Just i)    = snd . minimum $ l
                                          in if isLess then C i (a,b) else E (a,b) 

part2 :: Grid -> Int
part2 = length . filter inRange
    where
        inRange (C i (a,b)) = True
        inRange _ = False

main2 = do
        input <- lines <$> readFile "input.txt"
        let parsed    = map parse input
            m         = constructMap parsed
            finalGrid = map (calcPosition2 parsed m) (initialGrid parsed)
            res       = part2 finalGrid
        putStrLn . show $ res

{- 
 For the first part took me a while to brute force and find what limits
 would give me the correct answer.
-}
