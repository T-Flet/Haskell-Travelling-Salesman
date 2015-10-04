-- INTRODUCTION ---------------------------------------------------------------
--
--   Author:
--       Dr-Lord
--   Version:
--       0.2 03/10/2015
--
--   Repository:
--       https://github.com/Dr-Lord/Haskell-Travelling-Salesman
--
--   Description:
--      Program to solve or approximate a solution to a Travelling Salesman Problem
--      as explained in this repository:
--            https://github.com/GUTS2015/Glasgow-TSP-Challenge
--      This program implements the 2-opt optimisation algorithm for TSP.
--
--   Sections:
--       1 - Imports and Type declarations
--       2 - Testing Stuff
--       3 - To Do
--       4 - Main Functions
--       5 - Other Functions

---- 1 - IMPORTS AND TYPE DECLARATIONS -----------------------------------------

import System.Random (getStdGen, setStdGen, randomRs)
import Data.List (sort, delete)


  -- A point with its unique identity and coordinates
data Point = Point {pId :: Int, pX :: Int, pY :: Int} deriving (Eq, Ord, Read, Show)



---- 2 - TESTING STUFF ---------------------------------------------------------



---- 3 - TO DO -----------------------------------------------------------------

  -- Check paths with Verify, as there might be discrepancies in distance

  -- Build a mix of random and 2opt, generating as many initial values as there
  -- are threads and each working concurrently on his own file.
  -- Then, perhaps, a short function to compare them all.



---- 4 - MAIN FUNCTIONS --------------------------------------------------------

-- COMPILE: ghc -o 2opt -O 2opt
-- Or, Multi Core: ghc -o 2opt -O 2opt -threaded +RTS -N

  -- IO and processing structure
main = do
  pointsText <- readFile "g7k.tsp"
  --pointsText <- readFile "8.tsp"
  let points = extractPoints . map words $ lines pointsText
  let table = genDists points
  let pids = [0.. length points - 1]

  gen <- getStdGen
  setStdGen gen
  let randInds = randomRs (0, length points - 1) gen

----
----

  bestText <- readFile "2optSolution.txt"
  let best@(pLen,bestPids) = case lines bestText of
              [] -> let randPids = shuffle pids $ take (length pids) randInds in
                      (pathLength table randPids, randPids)
              x  -> read $ last x :: (Float,[Int])
  keepTrying points table bestPids randInds best


  -- Main loop function, printing on terminal and writing on file only better solutions than before
keepTrying :: [Point] -> [[Float]] -> [Int] -> [Int] -> (Float,[Int]) -> IO ()
keepTrying points table pids (ri1:ri2:newRandInds) best@(pLen,_) = do
  let [i1,i2] = sort [ri1, ri2]
  let iMax = length points - 1
  if smartShorterCheck table pids i1 i2 iMax
    then do
      let newPids = apply2opt pids i1 i2
      let newLength = pathLength table newPids
      let newBest = (newLength,newPids)
      --print newBest
      writeFile "2optSolution.txt" $ show newBest ++ "\n"
      keepTrying points table newPids newRandInds newBest
    else keepTrying points table pids newRandInds best


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


  -- Smartly check whether inverting a subset of a given path makes it shorter
  -- (As opposed to doing it and then measuring it whole again)
  -- Note that i1 must be <= i2
smartShorterCheck :: [[Float]] -> [Int] -> Int -> Int -> Int -> Bool
smartShorterCheck table pids i1 i2 iMax = (nd1 + nd2) < (d1 + d2)
  where nd1 = table!!in1Pid!!out2Pid
        nd2 = table!!out1Pid!!in2Pid

        d1 = table!!out1Pid!!in1Pid
        d2 = table!!in2Pid!!out2Pid

        [out1Pid,in1Pid,in2Pid,out2Pid] = map (pids!!) [out1,in1,in2,out2]
          -- The order above is how the points should be visualised:
          -- the "in" points are swapped and those between them reversed

        (out1,in1)
          | i1 == 0   = (iMax,   i1)
          | otherwise = (i1 - 1, i1)
        (in2,out2)
          | i2 == 0   = (iMax,   i2)
          | otherwise = (i2 - 1, i2)
          -- YES, like this, because i1 is the index of the first point in The
          -- subset, while i2 is the index of the first one AFTER the subset
          -- because they are used with splitAt; see apply2opt belowx


  -- Apply the 2-opt algorithm: randomly pick
  -- Note that i1 must be <= i2
  -- Note also that splitAt's argument is the index of the first element of the second output
apply2opt :: Eq a => [a] -> Int  -> Int -> [a]
apply2opt vals i1 i2 = before ++ reverse subset ++ after
  where (subset,after) = splitAt (i2 - i1) rest
        (before,rest)  = splitAt i1 vals


  -- Shuffle a list by splicing and modding a random list of integers to use as indexes
shuffle :: Eq a => [a] -> [Int] -> [a]
shuffle vals = fst . foldr shuf ([],vals)
  where shuf i (acc,rest) = (v:acc, delete v rest)
          where v = rest !! (i `mod` length rest)


  -- Get a string in the required format from a "best" value (as defined in keepTrying)
getString :: IO ()
getString = do
  bestText <- readFile "2optSolution.txt"
  let firstLine = head $ lines bestText
  let (_,best) = read firstLine :: (Float,[Int])
  writeFile "SOLUTION.txt" . unlines $ map show best



---- 5 - OTHER FUNCTIONS -------------------------------------------------------
