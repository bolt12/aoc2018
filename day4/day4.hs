module Day4 (
) where

import Control.Monad.State.Strict
import Control.Monad.IO.Class (liftIO)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

{-
    Advent of code - Day 2: Repose Record
    1st Part:
        Strategy 1: Find the guard that has the most minutes asleep. What minute does that guard spend asleep the most?
        What is the ID of the guard you chose multiplied by the minute you chose? 
-}

type Id = Int
type Minute = Int
type Day = Int
type TimeAsleep = Int
type TotalMinutes = Int
type Times = Int
type MinuteAtSleep = Map Minute (Times, TotalMinutes)
data G = G Id (Minute, MinuteAtSleep) TimeAsleep
       deriving (Show, Eq)
data Record = Guard G | Awake Minute | Asleep Minute
            deriving (Show, Eq)
type Guards = Map Id G
type GuardState a = State (G, Guards) a

(><) :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
(f >< g) (a, b) = (f a, g b)

parse :: String -> Record
parse s = let tokens = splitOn " " s
              minute = read . init . last . splitOn ":" $ tokens !! 1 :: Int
              (h:t)  = drop 2 tokens
              id     = read . tail . head $ t
              in
              case h of
                  "Guard" -> Guard $ G id (minute, M.empty) 0
                  "wakes" -> Awake minute
                  "falls" -> Asleep minute

record :: Record -> GuardState Guards
record (Guard (G i (m,mm) t)) = do
        (g, mp) <- get
        case M.lookup i mp of
            Nothing -> do
                modify (const (G i (m,mm) t) >< const (M.insert i (G i (m,mm) t) mp))
                return mp
            (Just (G i2 (m2,mm2) t2)) -> do
                modify (const (G i (m2,mm2) t2) >< id)
                return mp
record (Asleep m) = do
        (G i (_,mm) t, mp) <- get
        modify (const (G i (m,mm) t) >< const (M.update (const $ Just (G i (m,mm) t)) i mp))
        return mp
record (Awake m) = do
        (G i (m',mm) t, mp) <- get
        let timeAsleep = t + (m-m')
            newMinuteMap = updateAll m' (m-m') mm 
            in do
                modify (const (G i (m,newMinuteMap) timeAsleep) >< const (M.update (const $ Just (G i (m,newMinuteMap) timeAsleep)) i mp))
                return mp
    where
        updateAll _ 0 mm = mm
        updateAll m r mm = case M.lookup m mm of
                               Nothing -> updateAll (m+1) (r-1) (M.insert m (1, r) mm)
                               (Just (time,time2)) -> updateAll (m+1) (r-1) (M.update (const $ Just (time+1,time2+r)) m mm)

foldState :: [Record] -> (Record -> GuardState Guards) -> Guards
foldState (Guard (G i d t):ts) st = go ts st (G i d t, M.insert i (G i d t) M.empty)
        where
            go [] st (g,m) = m 
            go (r:rs) st s = go rs st (snd . runState (st r) $ s)

part1 :: Guards -> G
part1 = getRes . foldl1 maxG . map snd . M.toList
    where
        maxT a@(_, (t1, _)) b@(_, (t2, _)) = if t1 > t2 then a else b
        getRes (G i (m,mm) ta) = let newT = M.fromList . flip (:) [] . foldl1 maxT . M.toList $ mm
                                       in G i (m,newT) ta 
        maxG a@(G i1 h1 ta1) b@(G i2 h2 ta2) = if ta1 > ta2 then a else b

main1 = do
        input <- lines <$> readFile "s_input.txt"
        parsed <- return . map parse $ input
        res <- return . part1 $ foldState parsed record
        putStrLn . show $ res

{-
    2nd Part:
        Strategy 2: Of all guards, which guard is most frequently asleep on the same minute?
        What is the ID of the guard you chose multiplied by the minute you chose? 
-}

part2 :: Guards -> G
part2 = foldl1 maxGuard . map findFreq . map snd . M.toList
    where
        maxT a@(_, (t1, _)) b@(_, (t2, _)) = if t1 > t2 then a else b
        maxTB a@(_, (t1, _)) b@(_, (t2, _)) = if t1 > t2 then True else False
        findFreq (G i (m,h1) t) = let newH = M.fromList . flip (:) [] . foldl maxT (1,(-1,1)) . M.toList $ h1
                                        in G i (m,newH) t
        maxGuard a@(G _ (_,h1) _) b@(G _ (_,h2) _) = if maxTB (head . M.toList $ h1) (head . M.toList $ h2) then a else b

main2 = do
        input <- lines <$> readFile "s_input.txt"
        parsed <- return . map parse $ input
        res <- return . part2 $ foldState parsed record
        putStrLn . show $ res
{-
    For the first part the input was sorted by using the 'sort' bash command.

    I struggled a lot on this one, and as you can see the code isn't very clean. But
    the 'record' function I think it's easy to understand.

    I used the State monad because I thought it would be the best approach. But ended
    up with having to right the 'foldState' function myself because I could not find a
    pre defined function that did what I needed.

    The 'part1' and 'part2' functions are a bit messy because they're traversing the data structure
    and getting the result.

    NOTE: I got the first part right by accident since I did not have the 'updateAll' function
    when I ran the program. That function is the key to the exercise; it took me a while to understand
    that I needed to store all the intermediate minutes.
-}
