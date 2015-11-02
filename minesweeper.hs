import Data.Matrix

type Point a = (a, a)
type PointFn a = ((a -> a), (a -> a))

data Board a = Board { boardMin :: Point a,
                       boardMax :: Point a,
                       boardMines :: [Point a]}
               deriving (Show)

makeBoard :: Point Int -> [Point Int] -> Board Int
makeBoard dimensions mines = Board (0, 0) dimensions mines

neighborFns :: Enum a => [PointFn a]
neighborFns = [(pred, pred),
               (pred, id),
               (pred, succ),
               (succ, pred),
               (succ, id),
               (succ, succ),
               (id, succ),
               (id, pred)]

data Cell = Cell { cellMine :: Bool,
                   cellMineNeighbors :: Int,
                   cellVisible :: Bool,
                   cellFlagged :: Bool }

instance Show Cell where
  show cell
    | cellMine cell = "*"
    | cellMineNeighbors cell > 0 = show (cellMineNeighbors cell)
    | otherwise     = "."

emptyCell :: Cell
emptyCell = Cell False 0 False False

emptyProjection :: Board Int -> Matrix Cell
emptyProjection board = matrix (maxX - minX) (maxY - minY) $ \(i, j) -> emptyCell
                        where (minX, minY) = boardMin board
                              (maxX, maxY) = boardMax board

projectPoint :: Board Int -> Point Int -> Point Int
projectPoint board (x, y) = (succ (x - min_x), succ (y - min_y))
                            where (min_x, min_y) = boardMin board

addMineToProjection :: Matrix Cell -> Point Int -> Matrix Cell
addMineToProjection projection (r, c) = setElem cell' (r, c) projection
                                        where cell = getElem r c projection
                                              cell' = cell { cellMine = True }

neighbors :: (Integral a) => Point a -> Point a -> [Point a]
neighbors (max_x, max_y) (x, y) = filter inBounds (map (applyNeighborFn (x, y)) neighborFns)
                                  where applyNeighborFn (x, y) (fx, fy) = (fx x, fy y)
                                        inBounds (x, y) = (x >= 1) &&
                                                          (y >= 1) &&
                                                          (x <= max_x) &&
                                                          (y <= max_y)

addMineNeighborToProjection :: Matrix Cell -> Point Int -> Matrix Cell
addMineNeighborToProjection projection (r, c) = setElem cell' (r, c) projection
                                                where cell = getElem r c projection
                                                      cell' = cell { cellMineNeighbors = succ (cellMineNeighbors cell) }

addMineNeighborsToProjection :: Matrix Cell -> Point Int -> Matrix Cell
addMineNeighborsToProjection projection (r, c) = foldl addMineNeighborToProjection projection (neighbors dimensions (r, c))
                                                 where dimensions = (nrows projection, ncols projection)

projectMine :: Board Int -> Matrix Cell -> Point Int -> Matrix Cell
projectMine board projection (x, y) = addMineNeighborsToProjection (addMineToProjection projection (r, c)) (r, c)
                                      where (r, c) = projectPoint board (x, y)

project :: Board Int -> Matrix Cell
project board = foldl (projectMine board) (emptyProjection board) (boardMines board)

board = makeBoard (2, 3) [(0, 0)]
projection = project board
