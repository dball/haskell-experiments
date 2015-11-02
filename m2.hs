import Data.Matrix

updateElem :: Matrix a -> (a -> a) -> (Int, Int) -> Matrix a
updateElem m f (r, c) = setElem v' (r, c) m
                        where v' = f $ getElem r c m

data Cell = Cell { mine :: Bool,
                   nearbyMines :: Int,
                   visible :: Bool,
                   flagged :: Bool }

instance Show Cell where
  show cell
    | visible cell && mine cell  = "*"
    | visible cell && nearbyMines cell > 0 = show $ nearbyMines cell
    | visible cell = " "
    | flagged cell = "!"
    | otherwise = "?"

emptyCell = Cell False 0 False False

data Game = Game { board :: Matrix Cell,
                   outcome :: Maybe Bool  }

instance Show Game where
  show game = show $ board game

plantMine game point = foldl addNearbyMine game' nearby
  where game' = updateCell (\c -> c { mine = True }) game point
        nearby = nearbyPoints game point
        addNearbyMine = updateCell $ \c -> c { nearbyMines = succ $ nearbyMines c }

newGame rows cols mines =
  foldl plantMine game mines
  where game = Game board Nothing
        board = matrix rows cols $ \_ -> emptyCell

updateCell f game point = game { board = updateElem (board game) f point }

viewCell game point = getElem r c (board game)
                      where (r, c) = point

plantFlag = updateCell $ \c -> c { flagged = True }

retractFlag = updateCell $ \c -> c { flagged = False }

nearbyPoints game point =
  filter inbounds $ map apply deltas
  where inbounds = \(r, c) -> r > 0 && c > 0 && r <= rows && c <= cols
        rows = nrows (board game)
        cols = ncols (board game)
        deltas = [(pred, pred),
                  (pred, id),
                  (pred, succ),
                  (succ, pred),
                  (succ, id),
                  (succ, succ),
                  (id, pred),
                  (id, succ)]
        apply = \(fx, fy) -> (fx r, fy c)
        (r, c) = point

winning cell = (visible cell && (not . mine) cell) || (flagged cell && mine cell)

won game = all winning $ toList $ board game

explore game point
  | mine cell = game' { outcome = Just False }
  | nearbyMines cell == 0 = foldl explore game' nearby
  | otherwise = game'
  where game' = updateCell (\c -> c { visible = True }) game point
        cell = viewCell game' point
        nearby = filter (unexplored game) (nearbyPoints game point)
        unexplored = \game point -> not . visible $ viewCell game point
