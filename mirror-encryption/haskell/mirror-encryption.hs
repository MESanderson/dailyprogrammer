module Main where

import Data.Char
import Data.Map

data Coord = Coord {row :: Int, col :: Int, val :: String} deriving (Show, Eq, Ord)

max_mirror_size = 13

getMirrorIxs mirrorStrings =
  [Coord r c [a] | (r, l) <- zip ixs mirrorStrings,
               (c, a) <- zip ixs l, isMirror a]
  where isBack x = '\\' == x
        isFront x = '/' == x
        isMirror x = (isBack x) || (isFront x)
        ixs = [0..]

moveLeft :: Coord -> [Coord] -> Coord
moveLeft coord coordList 
  | newCoordList == [] = []
  | otherwise = last [coord | coord <- coordList, coordFilt coord]
  where coordFilt x = ((row x) == (row coord)) && ((col coord) > (col x))


moveRight :: Coord -> [Coord] -> Coord
moveRight coord coordList 
  | newCoordList == [] = []
  | otherwise =  head newCoordList
  where coordFilt x = ((row x) == (row coord)) && ((col coord) < (col x))
        newCoordList = [coord | coord <- coordList, coordFilt coord]


moveUp :: Coord -> [Coord] -> Coord
moveUp coord coordList 
  | newCoordList == [] = []
  | otherwise = last [coord | coord <- coordList, coordFilt coord]
  where coordFilt x = ((row x) > (row coord)) && ((col coord) == (col x))

moveDown :: Coord -> [Coord] -> Coord
moveDown coord coordList 
  | newCoordList == [] = []
  | otherwise = last [coord | coord <- coordList, coordFilt coord]
  where coordFilt x = ((row x) < (row coord)) && ((col coord) == (col x))

letterCoordLookup = Data.Map.fromList ltoc
  where ltoc = [((chr ix), Coord ix (-1) "right") | ix <- [65..78]] ++
                    [((chr ix), Coord max_mirror_size ix "up") | ix <- [78..90]] ++
                    [((chr ix), Coord (-1) ix "down") | ix <- [97..109]] ++
                    [((chr ix), Coord ix max_mirror_size "left") | ix <- [110..122]]

right = "right"
left = "left"
up = "up"
down = "down"

dirFlip coord
  | dir == left = Coord a b right
  | dir == right = Coord a b left
  | dir == up = Coord a b down
  | dir == down = Coord a b up
  where (Coord a b dir) = coord
                    
coordLetterLookup = Data.Map.fromList ctol
       where ctol = [(dirFlip b, a) | (a, b) <- Data.Map.toList letterCoordLookup]


getMirrorCoords startCoord mirrorIxs dir =
  | 
    where Coord _ _ startdir = startCoord
          dirFunc c
            | startdir == right = moveRight
            | startdir == left = moveLeft
            | startdir == up = moveUp
            | startdir == down = moveDown
          nextCoord = (dirFunc startDir) startCoord mirrorIxs
          nextDir = getNextDir c 


main = do
  putStrLn "HI"
