module Day1 where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.IntSet (IntSet)
import qualified Data.IntSet as Set

{-
    Advent of code - Day 1: Chronal Calibration
    1st Part:
        Starting with a frequency of zero, 
        what is the resulting frequency after all of the changes in frequency have been applied?
-}

parseInput :: String -> [Int]
parseInput = map (read . filter (/= '+')) . lines

main1 :: IO ()
main1 = do
        input <- parseInput <$> readFile "input.txt"
        putStrLn . show $ sum input 

{-
    1st Part:
        What is the first frequency your device reaches twice?

    NOTE: Might be needed to go through the input multiple times.
-}

findDuplicate :: ([String], [String]) -> Int -> Map Int () -> Int
findDuplicate ([], l) f m  = findDuplicate (l,l) f m
findDuplicate (x:xs,l) f m = if Map.lookup f m == Nothing
                                   then findDuplicate (xs,l) (f + read (filter (/= '+') x)) (Map.insert f () m)
                                   else f

main2 :: IO ()
main2 = do
        input <-  lines <$> readFile "input.txt"
        putStrLn . show $ findDuplicate (input, input) 0 Map.empty
        
{- 
    For the second part, instead of a Map I could use an IntSet and get more efficient solution.
    There's a 'cycle' function on Prelude. With it I could write cleaner code:

    --- Alternative ---

    import qualified Data.IntSet as S
    import Data.IntSet (IntSet)
    
    findDuplicate :: [String] -> Int -> IntSet -> Int
    findDuplicate (x:xs) f s = if Set.member f s
                                       then f
                                       else findDuplicate xs (f + read (filter (/= '+') x)) (Set.insert f s)

    main2 :: IO ()
    main2 = do
            input <-  lines <$> readFile "input.txt"
            putStrLn . show $ findDuplicate (cycle input) 0 Map.empty
-}
