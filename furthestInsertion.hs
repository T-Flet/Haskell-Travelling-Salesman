-- INTRODUCTION ---------------------------------------------------------------
--
--   Author:
--       Dr-Lord
--   Version:
--       0.1 04/10/2015
--
--   Repository:
--       https://github.com/Dr-Lord/Haskell-Travelling-Salesman
--
--   Description:
--      Program to solve or approximate a solution to a Travelling Salesman Problem
--      as explained in this repository:
--            https://github.com/GUTS2015/Glasgow-TSP-Challenge
--      This program implements the Furthest Insertion optimisation algorithm for TSP.
--
--   Sections:
--       1 - Imports and Type declarations
--       2 - Testing Stuff
--       3 - To Do
--       4 - Main Functions
--       5 - Other Functions

---- 1 - IMPORTS AND TYPE DECLARATIONS -----------------------------------------

import Data.List (sort, sortBy, minimumBy, maximumBy, (\\))
import Data.Function (on)


  -- A point with its unique identity and coordinates
data Point = Point {pId :: Int, pX :: Int, pY :: Int} deriving (Eq, Ord, Read, Show)



---- 2 - TESTING STUFF ---------------------------------------------------------



---- 3 - TO DO -----------------------------------------------------------------

  -- Check paths with Verify, as there might be discrepancies in distance

  -- Build a mix of random and 2opt, generating as many initial values as there
  -- are threads and each working concurrently on his own file.
  -- Then, perhaps, a short function to compare them all.



---- 4 - MAIN FUNCTIONS --------------------------------------------------------

-- COMPILE: ghc -o furthestInsertion -O furthestInsertion
-- Or, Multi Core: ghc -o furthestInsertion -O furthestInsertion -threaded +RTS -N

  -- IO and processing structure
main = do
  pointsText <- readFile "g7k.tsp"
  --pointsText <- readFile "8.tsp"
  let points = extractPoints . map words $ lines pointsText
  let table = genDists points
  let pids = [0.. length points - 1]

  let (maxDist,path) = getMaxDist table

  let bestPath = getBestPath table path (pids \\ path)

  writeFile "furthestInsertionSolution.txt" $ show bestPath
  return bestPath
  --return (maxDist,path)


  -- Furthest Insertion Implementation
getBestPath :: [[Float]] -> [Int] -> [Int] -> (Float,[Int])
getBestPath table path pids = (pathLength table finalPath, finalPath)
  where finalPath = foldr bestPidInsert path pids
        bestPidInsert pid path' = snd $ minimumBy (compare `on` fst) allPidPositions
          where allPidPositions = map (getNewPath path' pid) [1.. length path' - 1]

        getNewPath :: [Int] -> Int -> Int -> (Float,[Int])
        getNewPath pat pid ind = (nLen,nPids)
          where nLen = pathLength table nPids
                nPids = before ++ [pid] ++ after
                (before,after) = splitAt ind pat

          -- Checking whether the new path is shorter than the last can be done
          -- more efficiently by simply removing an edge and replacing it with
          -- two new ones; however it is not really worth it, as this whole method
          -- does not come as close as 2opt to a solution


  -- Make a list of points data structure from the equivalent text
extractPoints :: [[String]] -> [Point]
extractPoints = foldr pointify []
  where pointify :: [String] -> [Point] -> [Point]
        pointify [x,y] [] = [Point 0 (rI x) (rI y)]
        pointify [x,y] acc@(Point n _ _ : _) = Point (n+1) (rI x) (rI y) : acc
        rI = read :: String -> Int


  -- Generate a matrix of all distances between points
genDists :: [Point] -> [[Float]]
genDists ps = [[distance p1 p2 | p2 <- ps] | p1 <- ps]


  -- Euclidean distace
distance :: Point -> Point -> Float
distance (Point _ x1 y1) (Point _ x2 y2) = sqrt $ fromIntegral ((x2 - x1)^2 + (y2 - y1)^2)


  -- length of a path (lis ot pids)
pathLength :: [[Float]] -> [Int] -> Float
pathLength tab pids = len + tab!!firstPid!!lastPid
  where (len,lastPid,firstPid) = foldr sumUp (0,0,0) pids
        sumUp pid  (0,0,0)    = (0, pid, pid)
        sumUp pid2 (d,pid1,fp) = (d + tab!!pid1!!pid2, pid2, fp)


  -- Get a string in the required format from a "best" value (as defined in keepTrying)
getString :: IO ()
getString = do
  bestText <- readFile "furthestInsertionSolution.txt"
  let firstLine = head $ lines bestText
  let (_,best) = read firstLine :: (Float,[Int])
  writeFile "SOLUTION.txt" . unlines $ map show best



---- 5 - OTHER FUNCTIONS -------------------------------------------------------

 -- Return the indexes of the largest entry in a matrix
getMaxDist :: [[Float]] -> (Float,[Int])
getMaxDist table = (getDist path, path)
  where path = maximumBy comp [[i,j] | i <- [0.. length table - 1], j <- [0.. length table - 1]]
        comp = compare `on` getDist
        getDist [i,j] = table!!i!!j
