module Day2 (
) where

import Data.List (sort, group, any, (\\))

{-
    Advent of code - Day 2: Inventory Management System
    1st Part:
        What is the checksum for your list of box IDs?
-}

main1 = do
        input <- lines <$> readFile "input.txt"
        let parsed = map (map length . group . sort) input
            exactlyTwo = length . filter (any (== 2)) $ parsed
            exactlyThree = length . filter (any (== 3)) $ parsed
            res = exactlyTwo * exactlyThree
            in putStrLn . show $ res

{-
    2nd Part:
        What letters are common between the two correct box IDs?
-}

commons :: [String] -> [String]
commons p@(l:ls) = go l ls ls [""]
    where
        go _ [] [z] i = i
        go x [] (z:zs) i = go z (tail zs) zs i
        go x (y:ys) z i = let diff = x \\ y
                              in if length diff == 1 && (diff /= "")
                                     then go x ys z (filter (/= head diff) x:i)
                                     else go x ys z i

main2 = do
        input <- lines <$> readFile "input.txt"
        let common = commons input
            in putStrLn . show $ head common

{-
    For the second part I could have used list comprehensions. It would be much cleaner, like this:

    import Data.List

    commons input = head [a `intersect` b | a <- input, b <- input, length (a \\ b) == 1]
-}
