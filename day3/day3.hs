import Data.List.Split (splitOn)
import Data.List (splitAt, sort, group)
import Data.Set (Set)
import qualified Data.Set as Set

type Id = Int
type OffsetLeft = Int
type OffsetTop = Int
type Width = Int
type Height = Int

data Claim = Claim Id (OffsetLeft, OffsetTop) (Width,Height)
        deriving (Show, Eq)

{-
    Advent of code - Day 3: No Matter How You Slice It
    1st Part:
        How many square inches of fabric are within two or more claims?
-}

parse :: String -> Claim
parse x = let [id',_ , offset', area'] = splitOn " " x
              id = read . tail $ id'
              [l, t] = splitOn "," offset'
              [w, h] = splitOn "x" area'
              in Claim id (read l, read . init $ t) (read w, read h)

claim2Pos :: Claim -> [(Int, Int)]
claim2Pos (Claim id (l, t) (w, h)) = [(a+l, b+t) | a <- [0..(w-1)], b <- [0..(h-1)]]

main1 = do
        input <- lines <$> readFile "input.txt"
        parsed <- return $ map parse input
        positions <- return $ concatMap claim2Pos parsed
        res <- return . length . filter ((>1) . fst) . map ((,) <$> length <*> head) . group . sort $ positions
        putStrLn . show $ res

{-
    2nd Part:
        What is the ID of the only claim that doesn't overlap?
-}

checkOverlap :: Claim -> Set (Int,(Int,Int)) -> Maybe Id
checkOverlap c@(Claim id o p) = go (claim2Pos c)
    where
        go [] l = Just id
        go (p:ps) l = if Set.member (1,p) l
                            then go ps l
                            else Nothing

main2 = do
        input <- lines <$> readFile "input.txt"
        parsed <- return $ map parse input
        positions <- return $ concatMap claim2Pos parsed
        overlaps <- return . Set.fromList . filter ((==1) . fst) . map ((,) <$> length <*> head) . group . sort $ positions
        res <- return . filter (/= Nothing) . map (`checkOverlap` overlaps) $ parsed 
        putStrLn . show $ res

{-
    For the second part, a list makes it a lot slower, so the trick was to use a Set.

    Note: Set in Haskell is implemented as a balanced binary tree.
-}
