{-# LANGUAGE FlexibleContexts #-}

import           Control.Arrow        ((&&&), (***))
import           Data.Char            (isDigit, toUpper)
import           Data.Functor.Classes (compare2)
import qualified Data.List            as List
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Maybe           (catMaybes, fromJust)
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           System.Environment   (getArgs, lookupEnv)

type Problem = String

type Solution = String

type BoardSize = (Int, Int)

type Cell = (Int, Int)

type Score = Int

type Queue a = [(Int, a)]

main :: IO ()
main = do
  args <- getArgs
  interact $ eachLine $ calculateProblem $ getBoardSize args
  where
    getBoardSize :: [String] -> BoardSize
    getBoardSize args
      | length args == 0 = (8, 8)
      | length args == 1 = (read $ args !! 0, read $ args !! 0)
      | otherwise = (read $ args !! 0, read $ args !! 1)

-- |'eachLine' lazily runs a function 'op'.
eachLine :: (String -> String) -> (String -> String)
eachLine op = unlines . map op . lines

-- |'calculateProblem' finds a shortest path between a series of chess coordinates.
calculateProblem :: BoardSize -> Problem -> Solution
calculateProblem size = calculateValid . map getCoordinates . words
  where
    calculateValid :: [Cell] -> String
    calculateValid coordinates =
      if (all (isValidPosition size) coordinates) && length coordinates > 1
        then unwords $ map getChessLabels $ getShortestPaths coordinates
        else "INVALID"
    getShortestPaths :: [Cell] -> [Cell]
    getShortestPaths (first:coordinates) =
      fst $ List.foldl' accumulatePaths ([], first) coordinates
    accumulatePaths :: ([Cell], Cell) -> Cell -> ([Cell], Cell)
    accumulatePaths (results, from) to =
      (safeInit results ++ getShortestPath size from to, to)
    safeInit :: [a] -> [a]
    safeInit list =
      (if null list
         then []
         else init list)

-- |'getCoordinates' converts a chess label A1 into a Cell (0, 0).
getCoordinates :: String -> Cell
getCoordinates = getXCoordinate &&& getYCoordinate
  where
    getXCoordinate :: String -> Int
    getXCoordinate =
      fst .
      foldr sumWithAlphabetBase (0, 1) .
      catMaybes . map getMaybeLetterIndex . map toUpper
      where
        alphabet = ['A' .. 'Z']
        getMaybeLetterIndex :: Char -> Maybe Int
        getMaybeLetterIndex = flip List.elemIndex alphabet
        sumWithAlphabetBase :: Int -> (Int, Int) -> (Int, Int)
        sumWithAlphabetBase index (total, multiplier) =
          (total + index * multiplier, multiplier * length alphabet)
    getYCoordinate :: String -> Int
    getYCoordinate = subtract 1 . read . ("0" ++) . filter isDigit

-- |'getChessLabels' converts a Cell (0,0) into a chess label A1.
getChessLabels :: Cell -> String
getChessLabels (x, y) = getXChessLabel x ++ getYChessLabel y
  where
    getXChessLabel :: Int -> String
    getXChessLabel coordinate =
      map (\digit -> alphabet !! digit) $ getDigits [] coordinate
      where
        alphabet = ['A' .. 'Z']
        getDigits :: [Int] -> Int -> [Int]
        getDigits results coordinate = target ++ [remainder] ++ results
          where
            (result, remainder) = divMod coordinate $ length alphabet
            target =
              if result == 0
                then []
                else getDigits [] result
    getYChessLabel :: Int -> String
    getYChessLabel = show . (+ 1)

-- |'isValidPosition' checks if a Cell is in a valid position.
isValidPosition :: BoardSize -> Cell -> Bool
isValidPosition (width, height) (x, y) =
  x >= 0 && x < width && y >= 0 && y < height

-- |'getShortestPath' calculates the shortest path between two Cells.
getShortestPath :: BoardSize -> Cell -> Cell -> [Cell]
getShortestPath size from to =
  astar [(heuristic from, (from, 0))] Set.empty (Map.singleton from 0) Map.empty
  where
    astar ::
         Queue (Cell, Score)
      -> Set Cell
      -> Map Cell Score
      -> Map Cell Cell
      -> [Cell]
    astar queue seen scores parents
      | List.null queue = []
      | isEndNode currentNode = findPath parents currentNode
      | Set.member currentNode seen = astar (tail queue) seen scores parents
      | otherwise = astar nextQueue nextSeen nextScores nextParents
      where
        (currentNode, currentCost) = snd . head $ queue
        nextCost = currentCost + 1
        nextSeen = Set.insert currentNode seen
        nextNodes = List.filter isValidNextNode $ getNextNodes currentNode
        nextQueue = List.foldl' queueInsertNode (tail queue) nextNodes
        nextScores = List.foldl' (mapInsertValue nextCost) scores nextNodes
        nextParents = List.foldl' (mapInsertValue currentNode) parents nextNodes
        isValidNextNode :: Cell -> Bool
        isValidNextNode node = isUnseen node && isLowerScore node
        isUnseen :: Cell -> Bool
        isUnseen node = not $ Set.member node nextSeen
        isLowerScore :: Cell -> Bool
        isLowerScore node =
          not (Map.member node scores) || nextCost < getNodeScore node
        getNodeScore :: Cell -> Score
        getNodeScore node = fromJust . Map.lookup node $ scores
        queueInsertNode :: Queue (Cell, Score) -> Cell -> Queue (Cell, Score)
        queueInsertNode queue node =
          List.insertBy compare2 (getQueuedNode node) queue
        getQueuedNode :: Cell -> (Int, (Cell, Score))
        getQueuedNode node = (nextCost + heuristic node, (node, nextCost))
        mapInsertValue value map node = Map.insert node value map
        findPath :: Map Cell Cell -> Cell -> [Cell]
        findPath parents node =
          if Map.member node parents
            then findPath parents (getNodeParent node) ++ [node]
            else [node]
        getNodeParent :: Cell -> Cell
        getNodeParent node = fromJust . Map.lookup node $ parents
    isEndNode :: Cell -> Bool
    isEndNode = (==) to
    getNextNodes :: Cell -> [Cell]
    getNextNodes (x, y) =
      List.filter (isValidPosition size) $
      [ (x + 2, y + 1)
      , (x + 2, y - 1)
      , (x - 2, y + 1)
      , (x - 2, y - 1)
      , (x + 1, y + 2)
      , (x + 1, y - 2)
      , (x - 1, y + 2)
      , (x - 1, y - 2)
      ]
    heuristic :: Cell -> Int
    heuristic (x, y) = isqrt $(toX - x) ^ 2 + (toY - y) ^ 2
      where
        (toX, toY) = to
        isqrt :: Int -> Int
        isqrt = floor . sqrt . fromIntegral

-- |'generateTestData' generates all the possible problems between two Cells.
generateTestData :: BoardSize -> String
generateTestData (width, height) = init . unlines $ allMoves
  where
    allCells =
      map getChessLabels $ (map (,) [0 .. width - 1]) <*> [0 .. height - 1]
    allMoves = (map spaceJoin allCells) <*> allCells
    spaceJoin a b = a ++ " " ++ b
