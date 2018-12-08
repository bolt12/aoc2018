
import Data.Map (Map)
import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.List (sort, nub, (\\))

type Graph a = Map a [a]

{-
    Advent of code - Day 7: The Sum of Its Parts
    1st Part:
        In what order should the steps in your instructions be completed?
-}

parse :: String -> Graph Char -> Graph Char
parse s g = let tokens = splitOn " " s
                fst    = head $ tokens !! 1
                snd    = head $ tokens !! 7
                in case M.lookup fst g of
                       Nothing  -> M.insert fst [snd] g
                       (Just a) -> let sortedAdjacents = sort (snd:a)
                                       in M.update (const $ Just sortedAdjacents) fst g

addTerminals :: Graph Char -> Graph Char
addTerminals g = let l         = M.toList g
                     keys      = map fst l
                     values    = nub . concatMap snd $ l
                     terminals = values \\ keys
                     in foldr (`M.insert` []) g terminals

next1 :: Graph Char -> ([Char], Graph Char)
next1 g = let l     = M.toList g
              keys   = map fst l
              values = concatMap snd l
              nexts  = sort . filter (not . flip elem values) $ keys
              newMap = if null nexts then g else M.delete (head nexts) g
              in (nexts, newMap)

part1 :: Graph Char -> String
part1 g = let (nexts, g2) = next1 g
              in if null nexts 
                     then [] 
                     else head nexts:part1 g2
                      
main1 = do
        input <- lines <$> readFile "input.txt"
        let graph         = foldr parse M.empty input
            withTerminals = addTerminals graph
            in
                putStrLn . part1 $ withTerminals

{-
    2nd Part:
        With 5 workers and the 60+ second step durations described above, how long will it take to complete all of the steps?
-}

type TimeLeft = Int
data Worker  = W Char TimeLeft -- '.' is idle, independently of the TimeLeft 
             deriving (Eq, Show)
type Workers = [Worker]

initialWorkers :: Workers
initialWorkers = replicate 5 (W '.' (-1))

next2 :: Graph Char -> Workers -> ([Char], Workers, Graph Char)
next2 g w = let upWorkers  = updateW w
                results    = sort . collect $ upWorkers
                newGraph   = if null results then g else foldr M.delete g results
                l          = M.toList newGraph
                keys       = map fst l
                values     = concatMap snd l
                nexts      = sort . filterNexts upWorkers . filter (not . flip elem values) $ keys
                newWorkers = distribute nexts upWorkers 
                in (results, newWorkers, newGraph)
    where
        filterNexts [] nexts              = nexts
        filterNexts (W '.' (-1):ws) nexts = filterNexts ws nexts
        filterNexts (W c _:ws) nexts      = filterNexts ws (filter (/= c) nexts)

        collect []         = []
        collect (W c 0:ws) = c:collect ws
        collect (W _ t:ws) = collect ws

        updateW []              = []
        updateW (W c 0:ws)      = W '.' (-1):updateW ws
        updateW (W '.' (-1):ws) = W '.' (-1):updateW ws
        updateW (W c t:ws)      = W c (t-1):updateW ws

        distribute _ []                = []
        distribute [] ws               = ws
        distribute (n:ns) (W '.' _:ws) = W n (fromEnum n - 4):distribute ns ws
        distribute (n:ns) (W c t:ws)   = W c t:distribute (n:ns) ws

part2 :: Graph Char -> Workers -> Int -> Int
part2 g w t = let (nexts, w2, g2) = next2 g w
              in if M.null g2
                     then t
                     else part2 g2 w2 (t+1)

main2 = do
        input <- lines <$> readFile "input.txt"
        let graph         = foldr parse M.empty input
            withTerminals = addTerminals graph
            workers       = initialWorkers
            in
                putStrLn . show . part2 withTerminals workers $ 0

{-
    I saw many elaborate solutions on /r/adventofcode regarding Haskell
    but I think this one is pretty strightforward on what the problem asks.

    Curiosly for part2 the traversing is correct for the sample input but the 
    total time is off by 3. I gave it a shot at my actual input and got it right!
-}
