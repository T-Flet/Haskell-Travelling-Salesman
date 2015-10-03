-- INTRODUCTION ---------------------------------------------------------------
--
--   Author:
--       Dr-Lord
--   Version:
--       0.1 03/10/2015
--
--   Repository:
--       https://github.com/Dr-Lord/Haskell-Travelling-Salesman
--
--   Description:
--      Program to solve or approximate a solution to a Travelling Salesman Problem
--      as explained in this repository:
--            https://github.com/GUTS2015/Glasgow-TSP-Challenge
--      This program generates random paths and keeps track of the shortest so far,
--      printing any new one both on terminal and to file.
--
--   Sections:
--       1 - Imports and Type declarations
--       2 - Testing Stuff
--       3 - To Do
--       4 - Main Functions
--       5 - Other Functions

---- 1 - IMPORTS AND TYPE DECLARATIONS -----------------------------------------

import System.Random (getStdGen, setStdGen, randomRs)


  -- A point with its unique identity and coordinates
data Point = Point {pId :: Int, pX :: Int, pY :: Int} deriving (Eq, Ord, Read, Show)



---- 2 - TESTING STUFF ---------------------------------------------------------



---- 3 - TO DO -----------------------------------------------------------------



---- 4 - MAIN FUNCTIONS --------------------------------------------------------

  -- IO and processing structure
main = do
  --pointsText <- readFile "g7k.tsp"
  pointsText <- readFile "test12.txt"
  let points = extractPoints . map words . reverse $ lines pointsText
  let table = genDists points

  let pids = [0.. length points - 1]
  gen <- getStdGen
  setStdGen gen
  let randInds = randomRs (0, length points - 1) gen

  bestText <- readFile "randomSolution.txt"
  let best = case lines bestText of
              [] -> (pathLength table pids, pids)
              x  -> read $ last x
  keepTrying points table pids randInds best


  -- Main loop function, printing on terminal and writing on file only better solutions than before
keepTrying :: [Point] -> [[Float]] -> [Int] -> [Int] -> (Float,[Int]) -> IO ()
keepTrying points table pids randInds best@(pLen,_) = do
  let newRandInds = take (length pids) randInds
  let newPids = permute pids newRandInds
  let newLength = pathLength table newPids

  if newLength < pLen
    then let newBest = (newLength,newPids) in do
      print newBest
      appendFile "randomSolution.txt" $ show newBest ++ "\n"
      keepTrying points table newPids newRandInds newBest
    else keepTrying points table newPids newRandInds best


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
pathLength tab = fst . foldr sumUp (0,0)
  where sumUp pid  (0,0)    = (0,pid)
        sumUp pid2 (d,pid1) = (d + tab!!pid1!!pid2, pid2)


  -- Permutation of a list following an index swap list, with each new value
  -- being the one which previousl was at index n.
  -- The inpits are the list and a list of indexes (of the same length, obviously)
permute :: [Int] -> [Int] -> [Int]
permute vals = map (vals!!)



---- 5 - OTHER FUNCTIONS -------------------------------------------------------
