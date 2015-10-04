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
--
--   Sections:
--       1 - Imports and Type declarations
--       2 - Testing Stuff
--       3 - To Do
--       4 - Main Functions
--       5 - Other Functions

---- 1 - IMPORTS AND TYPE DECLARATIONS -----------------------------------------

import Data.List (sortBy, (\\), delete, intersect, minimumBy, nub, partition)
import Data.Function (on)


  -- A point with its unique identity and coordinates
data Point = Point {pId :: Int, pX :: Int, pY :: Int} deriving (Eq, Ord, Read, Show)

  -- An Edge with a length and defined by two Points
data Edge  = Edge  {eLen :: Float, eP1 :: Point, eP2 :: Point} deriving (Eq, Ord, Show, Read)



---- 2 - TESTING STUFF ---------------------------------------------------------



---- 3 - TO DO -----------------------------------------------------------------

-- Since there are many small loops in the obtained result so far, isolate them,
-- and then go through the remaining edges looking for the shortest between elements
-- of each group in order toconnect them all.
  --  NOTE: Use the remaining edges in the last shortlist before extending it,
  --        as they are shorter, XD
  --  NOTE: Eventually consider both the length of the edges which are going to be
  --        replaced in order to join loops and the length of the edges which are
  --        going to join them; then only operate on the smallest differences


-- Perhaps consider using noSuperest to clear remainging edges of done points


-- Set up a random path generator program and leave it running until the end, printing
-- out only better paths than the best one so far in a separate shell


-- Set up a 2-opt
-- Set up a Furthest Insertion one



---- 4 - MAIN FUNCTIONS --------------------------------------------------------

  -- IO and processing structure
main = do
  pointsText <- readFile "g7k.tsp"
  let points = extractPoints . map words . reverse $ lines pointsText
  let edges  = sortBy (compare `on` eLen) $ extractLengths points
  let (possLoop,rest) = spanPossLoop edges
  let (open,loops) = partitionFragments $ linkPossEdges possLoop
  return $ foldl (\\) possLoop (open ++ loops)
  --return $ map (map pId . pointsIn) open
  --return $ map (map pId . pointsIn) loops
  --return $ linkPossEdges possLoop
  --return $ lengthAndPerm possLoop
  --return possLoop
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


  -- Return the smallest unsorted list of Edges which could constitute a single loop
  -- I.e. each point is present at least in 2 of them, and at least one in exactly 2
spanPossLoop :: [Edge] -> ([Edge],[Edge])
spanPossLoop = needPoints [] ([0..347] ++ [0..347])
  where needPoints :: [Edge] -> [Int] -> [Edge] -> ([Edge],[Edge])
        needPoints _   _     [] = error "This is bad"
        needPoints acc []    es = (acc, es)
        needPoints acc pids (e@(Edge _ (Point pid1 _ _) (Point pid2 _ _)):es)
          | [pid1,pid2] `subsetOf` pids = needPoints (e:acc) (pids \\ [pid1,pid2]) es
          | otherwise = needPoints acc pids es


  -- Link together all possible Edges in a set, resulting in both open and closed fragments
linkPossEdges :: [Edge] -> [[Edge]]
linkPossEdges = getNextPid []
  where getNextPid :: [[Edge]] -> [Edge] -> [[Edge]]
        getNextPid [] (e:es) = getNextPid [[e]] es
        getNextPid acc []    = acc
        getNextPid accAcc@(acc@(ae:_):accs) ees@(e:es) =
            case filter (pidsInCommon . getPids) ees of
              []   -> getNextPid ([e]:accAcc) es
              [ne] -> getNextPid ((ne:acc):accs) (delete ne es)
              nes  -> getNextPid ((minNe:acc):accs) (delete minNe es)
                where minNe = minimumBy (compare `on` eLen) nes
          where pidsInCommon = not . null . intersect (getPids ae)


  -- Join up a set of fragments, be they open or closed
joinUpFragments :: ([[Edge]],[[Edge]]) -> ([Edge],[Edge]) -> [Edge]
joinUpFragments (open,loops) (possLoop,rest) = findLinks [] (open,loops) (reducedpossLoop ++ rest)
  where reducedPossLoop = foldl (\\) possLoop (open ++ loops)
        findLinks :: ([[Edge]],[[Edge]]) -> [Edge] -> [Edge]
        findLinks ([],[l]) _ = l
        findLinks ([],(l:los)) es =
        findLinks (oops@(o:ops),llos@(l:los)) escase kind of
          "oo" -> findLinks
          "ol" ->
          "lo" ->
          "ll" ->
          |     = findLinks (nop:op,lo) es
          |      = findLinks (op,lo) es
            where jointUp = 
                  bestRes@(kind,(bd,be,bx)) = sortBy (compare `on` (\(_,(x,_,_))->x)) [coo, col, clo, cll]
                  coo@(oobd,[oobe],oobop) = ("oo", closestOpOps o ops )
                  col@(olbd,[olbe],olbop) = ("ol", closestOpLos o llos)
                  clo@(lobd,[lobe],lobop) = ("lo", closestLoOps l oops)
                  cll@(llbd,[llbe],llbop) = ("ll", closestLoLos l los )

closestOpOps, closestOpLos, closestLoOps, closestLoLos :: [Edge] -> [[Edge]] -> (Float,[Edge],[Edge])
closestOpOps op ops = foldr measure (99999999,[],[]) ops
  where measure :: [Edge] -> (Float,[Edge],[Edge]) -> (Float,[Edge],[Edge])
        measure nop (_,[],[]) = measureOpNop op nop
        measure nop acc@(bd,[be],bop) = let nacc@(nd,_,_) = measureOpNop op nop in
                                            if nd < d then nacc else acc
closestOpLos o llos
closestLoOps l oops
closestLoLos l los


  -- Return the length of a loop and its corresponding permutation of points
--lengthAndPerm :: [Edge] -> (Float,[Int])
--lengthAndPerm lp = (l, perm)
--  where l = sum $ map eLen lp
--        perm = getNextPid [] $ map getPids lp
--        getNextPid :: [[Int]] -> [[Int]] -> [[Int]]
--        getNextPid [] ([pid1,pid2]:ePids) = getNextPid [pid1,pid2] ePids
--        getNextPid acc [] = acc
--        getNextPid acc@(lastPid:_) ePids = getNextPid (newPid:acc) (delete pidPair ePids)
--          where [newPid] = delete lastPid pidPair
--                pidPair = case filter (lastPid `elem`) ePids of
--                  [] ->



---- 5 - OTHER FUNCTIONS -------------------------------------------------------

  -- Check whether a Point is or is not in an Edge
--isIn, isNotIn :: Int -> Edge -> Bool
--pid `isIn`    (Edge _ p1 p2) = (pid == pId p1) || (pid == pId p2)
--pid `isNotIn` (Edge _ p1 p2) = (pid /= pId p1) && (pid /= pId p2)


  -- Extract the pids of the Points of an Edge
getPids :: Edge -> [Int]
getPids (Edge _ (Point pid1 _ _) (Point pid2 _ _)) = [pid1,pid2]


  -- Return the Points in a list of Edges
pointsIn :: [Edge] -> [Point]
pointsIn = nub . accPoints []
  where accPoints [] (Edge _ p1 p2 :es) = accPoints [p1,p2] es
        accPoints acc [] = acc
        accPoints ps (Edge _ p1 p2 :es) = accPoints (p1:p2:ps) es

  -- Return a string of Point unique identites one per line
stringify :: [Edge] -> String
stringify = unlines . map show . foldr orderPids []
  where orderPids (Edge _ (Point pid1 _ _) (Point pid2 _ _)) [] = [pid1,pid2]
        orderPids (Edge _ (Point pid1 _ _) (Point pid2 _ _)) acc@(lastPid:_)
          | pid1 == lastPid = pid2:acc
          | otherwise       = pid1:acc


  -- Partition path fragments into open ones and loops; it returns (open,loops)
partitionFragments :: [[Edge]] -> ([[Edge]],[[Edge]])
partitionFragments = partition isOpen
isOpen :: [Edge] -> Bool
isOpen [e]     = True
isOpen [e1,e2] = True
isOpen (e:es)  = null . (intersect `on` getPids) e $ last es


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
