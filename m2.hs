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

data Game = Game { board :: Matrix Cell }

instance Show Game where
  show game = show $ board game

newGame rows cols = Game $ matrix rows cols $ \_ -> emptyCell

plantFlag game point = updateElem cells addFlag point
                       where cells = board game
                             addFlag = (\c -> c { flagged = True })

retractFlag game point = updateElem cells removeFlag point
                         where cells = board game
                               removeFlag = (\c -> c { flagged = False })
                               
