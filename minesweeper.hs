import Data.Matrix

type Point a = (a, a)
type PointFn a = ((a -> a), (a -> a))

data Board a = Board { boardMin :: Point a,
                       boardMax :: Point a,
                       boardMines :: [Point a]}
               deriving (Show)

makeBoard :: Point Int -> Board Int
makeBoard point = Board (0, 0) point []

neighborFns :: Enum a => [PointFn a]
neighborFns = [(pred, pred),
               (pred, id),
               (pred, succ),
               (succ, pred),
               (succ, id),
               (succ, succ),
               (id, succ),
               (id, pred)]

neighbors :: (Enum a, Ord a) => Board a -> Point a -> [Point a]
neighbors board point = filter inBounds (map (applyNeighborFn point) neighborFns)
                        where applyNeighborFn (x, y) (fx, fy) = (fx x, fy y)
                              inBounds (x, y) = (x >= minX) &&
                                                (y >= minY) &&
                                                (x <= maxX) &&
                                                (y <= maxY)
                              (minX, minY) = boardMin board
                              (maxX, maxY) = boardMax board

data Cell = Cell { cellMine :: Bool,
                   cellMineNeighbors :: Int,
                   cellVisible :: Bool,
                   cellFlagged :: Bool }

instance Show Cell where
  show (Cell { cellMine = False }) = "."
  show (Cell { cellMine = True }) = "*"

emptyCell :: Cell
emptyCell = Cell False 0 False False

emptyProjection :: Board Int -> Matrix Cell
emptyProjection board = matrix (maxX - minX) (maxY - minY) $ \(i, j) -> emptyCell
                        where (minX, minY) = boardMin board
                              (maxX, maxY) = boardMax board

projectMine :: Board Int -> Matrix Cell -> Point Int -> Matrix Cell
projectMine board projection (mineX, mineY) = setElem cell' (x, y) projection
                                              where cell = getElem x y projection
                                                    (minX, minY) = boardMin board
                                                    x = succ (mineX - minX)
                                                    y = succ (mineY - minY)
                                                    cell' = cell { cellMine = True }

project :: Board Int -> Matrix Cell
project board = foldl (projectMine board) (emptyProjection board) (boardMines board)

board = makeBoard (2, 3)
projection = emptyProjection board
