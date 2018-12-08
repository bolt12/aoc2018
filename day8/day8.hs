
{-
    Advent of code - Day 8: Memory Maneuver
    1st Part:
        What is the sum of all metadata entries?
-}

type NChilds = Int
type NMetaD = Int
type MetaD = Int
data Node = Node NChilds NMetaD [Node] [MetaD]
          deriving (Show, Eq)

parse :: String -> Node
parse s = let (h:m:t) = map read . words $ s :: [Int]
              in fst $ go h m t
    where
        go 0 nMeta t       = (Node 0 nMeta [] (take nMeta t), drop nMeta t)
        go nChilds nMeta t = let (childs, end) = getChilds nChilds t
                                       in (Node nChilds nMeta childs (take nMeta end), drop nMeta end)
        
        getChilds 0 t             = ([], t)
        getChilds _ []            = ([], [])
        getChilds nChilds (0:m:t) = let (nextChildren, end) = getChilds (nChilds-1) (drop m t)
                                        in (Node 0 m [] (take m t):nextChildren, end)
        getChilds nChilds (h:m:t) = let (node, end) = go h m t
                                        (nextChildren, end2) = getChilds (nChilds-1) end
                                        in (node:nextChildren, end2)

part1 :: Node -> Int
part1 (Node _ _ ns metas) = sum metas + sum (map part1 ns)

main1 = do
        input <- readFile "input.txt"
        print . part1 . parse $ input

{-
    2nd Part:
        What is the value of the root node?
-}

part2 :: Node -> Int
part2 (Node n _ ns metas) = let toVisit = zipWith myIndex (repeat ns) metas 
                                in go toVisit
    where
        myIndex [] _ = Nothing
        myIndex (x:xs) 1 = Just x
        myIndex (x:xs) i = myIndex xs (i-1)

        go [] = 0
        go (Just (Node 0 _ _ metas):ns) = sum metas + go ns
        go (Just (Node n _ ns' metas):ns) = let toVisit = zipWith myIndex (repeat ns') metas
                                               in go toVisit + go ns
        go (Nothing:ns) = go ns

main2 = do
        input <- readFile "input.txt"
        print . part2 . parse $ input

{-
    I saw many solutions using parsing libraries to get through the input.
    I'm happy with my solution and data structure choice!
-}
