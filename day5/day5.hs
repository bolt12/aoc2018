
import Data.Char (ord, chr, toUpper)
import Data.Set (Set)
import qualified Data.Set as S

{-
    Advent of code - Day 5: Alchemical Reduction
    1st Part:
        How many units remain after fully reacting the polymer you scanned?
-}

cancel :: Char -> String -> String
cancel x [] = [x]
cancel x (y:ys) | ord x == ord y + 32 || ord x == ord y - 32 = ys
                | otherwise = x:y:ys

part1 :: String -> Int
part1 = length . foldr cancel ""

main1 = do
        input <- readFile "input.txt"
        putStrLn . show . subtract 1 . part1 $ input -- subtract 1: Because of \n

{-
    2nd Part:
        What is the length of the shortest polymer you can produce by removing all units of exactly one type and fully reacting the result?
-}

constructSet :: String -> Set Char
constructSet = foldr (\c s -> S.insert (toUpper c) s) S.empty

checkLength :: Char -> String -> Int
checkLength c = part1 . filter (\x -> toUpper x /= c && x /= '\n')

part2 :: String -> Set Char -> Int
part2 s = minimum . map (`checkLength` s) . S.toList

main2 = do
        input <- readFile "input.txt"
        putStrLn . show . part2 input $ constructSet input

{-
    It took me a while to figure out that the '\n' was messing my correct output.
-}
