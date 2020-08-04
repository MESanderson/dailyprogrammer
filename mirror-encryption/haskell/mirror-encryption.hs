module Main where

data Coord = Coord {row :: Int, col :: Int, val :: String} deriving (Show)

getMirrorIxs mirrorStrings =
  [Coord r c [a] | (r, l) <- zip ixs mirrorStrings,
               (c, a) <- zip ixs l, isMirror a]
  where isBack x = '\\' == x
        isFront x = '/' == x
        isMirror x = (isBack x) || (isFront x)
        ixs = [0..]

moveLeft :: Coord -> [Coord] -> Coord
moveLeft coord coordList =
  last [coord | coord <- coordList, coordFilt coord]
  where coordFilt x = ((row x) == (row coord)) && ((col coord) > (col x))


moveRight :: Coord -> [Coord] -> Coord
moveRight coord coordList =
  head [coord | coord <- coordList, coordFilt coord]
  where coordFilt x = ((row x) == (row coord)) && ((col coord) < (col x))


moveUp :: Coord -> [Coord] -> Coord
moveUp coord coordList =
  last [coord | coord <- coordList, coordFilt coord]
  where coordFilt x = ((row x) > (row coord)) && ((col coord) == (col x))

moveDown :: Coord -> [Coord] -> Coord
moveDown coord coordList =
  last [coord | coord <- coordList, coordFilt coord]
  where coordFilt x = ((row x) < (row coord)) && ((col coord) == (col x))

main = do
  putStrLn "HI"
