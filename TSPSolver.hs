-- INTRODUCTION ---------------------------------------------------------------
--
--   Author:
--       Dr-Lord
--   Version:
--       0.1 02/10/2015
--
--   Repository:
--       https://github.com/Dr-Lord/Haskell-Travelling-Salesman
--
--   Description:
--      Program to solve or approximate a solution to a Travelling Salesman Problem
--      as explained in this repository:
--            https://github.com/GUTS2015/Glasgow-TSP-Challenge
--
--   Sections:
--       1 - Imports and Type declarations
--       2 - Testing Stuff
--       3 - To Do
--       4 - Main Functions
--       5 - Other Functions

---- 1 - IMPORTS AND TYPE DECLARATIONS -----------------------------------------

import Data.List (sortBy, (\\), delete, intersect)
import Data.Function (on)


  -- A point with its unique identity and coordinates
data Point = Point {pId :: Int, pX :: Int, pY :: Int} deriving (Eq, Ord, Read, Show)

  -- An Edge with a length and defined by two Points
data Edge  = Edge  {eLen :: Float, eP1 :: Point, eP2 :: Point} deriving (Eq, Ord, Show, Read)



---- 2 - TESTING STUFF ---------------------------------------------------------



---- 3 - TO DO -----------------------------------------------------------------



---- 4 - MAIN FUNCTIONS --------------------------------------------------------

  -- IO and processing structure
main = do
  pointsText <- readFile "g7k.tsp"
  let points = extractPoints . map words . reverse $ lines pointsText
  let edges  = sortBy (compare `on` eLen) $ extractLengths points
  let shortLoop = makeLoop edges
  --return $ lengthAndPerm shortLoop
  return shortLoop
  --return . length $ enoughEdges edges
  --return $ takeWhile ((==0.0) . eLen) edges
  --return $ length points




  -- Make a list of points data structure from the equivalent text
extractPoints :: [[String]] -> [Point]
extractPoints = foldr pointify []
  where pointify :: [String] -> [Point] -> [Point]
        pointify [x,y] [] = [Point 0 (rI x) (rI y)]
        pointify [x,y] acc@(Point n _ _ : _) = Point (n+1) (rI x) (rI y) : acc
        rI = read :: String -> Int


  -- Return all the possible Edges from a list of Points
extractLengths :: [Point] -> [Edge]
extractLengths = map edgify . combinations 2
  where edgify [p1@(Point _ x1 y1),p2@(Point _ x2 y2)] = Edge dist p1 p2
          where dist = sqrt $ fromIntegral ((x2 - x1)^2 + (y2 - y1)^2)


  -- Return as many edges as needed to be able to make a loop
  -- I.e. As many as are required until each point comes up at least twice in them
--enoughEdges :: [Edge] -> [Edge]
--enoughEdges = checkPoints [] ([0..347] ++ [0..347])
--  where checkPoints :: [Edge] -> [Int] -> [Edge] -> [Edge]
--        checkPoints acc [] _ = acc
--        checkPoints acc pids (e@(Edge _ (Point pid1 _ _) (Point pid2 _ _)):es) =
--            checkPoints (e:acc) (pids \\ [pid1,pid2]) es


  -- Return an (unsorted but closed) loop (list of Edges) from a list of Edges
  -- with each point present at least in 2 of them
makeLoop :: [Edge] -> [Edge]
makeLoop = needPoints [] ([0..347] ++ [0..347])
  where needPoints :: [Edge] -> [Int] -> [Edge] -> [Edge]
        needPoints _ _ [] = error "This is bad"
        needPoints acc [] _ = acc
        needPoints acc pids (e@(Edge _ (Point pid1 _ _) (Point pid2 _ _)):es)
          | [pid1,pid2] `subsetOf` pids = needPoints (e:acc) (pids \\ [pid1,pid2]) es
          | otherwise = needPoints acc pids es


  -- Return the length of a loop and its corresponding permutation of points
lengthAndPerm :: [Edge] -> (Float,[Int])
lengthAndPerm lp = (l, perm)
  where l = sum $ map eLen lp
        perm = getNextPid [] $ map getPids lp
        getNextPid :: [Int] -> [[Int]] -> [Int]
        getNextPid [] ([pid1,pid2]:ePids) = getNextPid [pid1,pid2] ePids
        getNextPid acc [] = acc
        getNextPid acc@(lastPid:_) ePids = getNextPid (newPid:acc) (delete pidPair ePids)
          where [newPid] = delete lastPid pidPair
                [pidPair] = filter (lastPid `elem`) ePids



---- 5 - OTHER FUNCTIONS -------------------------------------------------------

  -- Check whether a Point is or is not in an Edge
isIn, isNotIn :: Int -> Edge -> Bool
pid `isIn`    (Edge _ p1 p2) = (pid == pId p1) || (pid == pId p2)
pid `isNotIn` (Edge _ p1 p2) = (pid /= pId p1) && (pid /= pId p2)


  -- Extract the pids of the Points of an Edge
getPids :: Edge -> [Int]
getPids (Edge _ (Point pid1 _ _) (Point pid2 _ _)) = [pid1,pid2]



-- Other Functions

  -- Check whether a set is a subset of a second set
subsetOf, notSubsetOf :: (Eq a) => [a] -> [a] -> Bool
a `subsetOf` b    = all (`elem` b)    a
a `notSubsetOf` b = any (`notElem` b) a


  -- Classic mathematical function
choose :: Integral a => a -> a -> a
infixl 5 `choose`
n `choose` k = fromIntegral $ product [sk+1..sn] `div` product [1..sn-sk]
  where [sn,sk] = map fromIntegral [n,k]


  -- Same function as in my GeneralFunctions package
combinations :: Int -> [a] -> [[a]]
combinations k xs = combinations' (length xs) k xs
  where combinations' _ _  [] = []
        combinations' n k' yys@(y:ys)
          | k' == 0   = [[]]
          | k' >= n   = [yys]
          | otherwise = map (y:) nkMinus1 ++ nMinus1
            where nkMinus1 = combinations' (n-1) (k'-1) ys
                  nMinus1  = combinations' (n-1)  k'    ys
