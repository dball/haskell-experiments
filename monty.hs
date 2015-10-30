import System.Random

data Door = One | Two | Three
            deriving (Eq, Ord, Show, Read, Bounded, Enum)

choose :: StdGen -> [a] -> (Maybe a, StdGen)
choose g [] = (Nothing, g)
choose g list = (Just (list !! index), g')
                where (index, g') = randomR (0, pred (length list)) g

reveal :: StdGen -> Door -> Door -> (Maybe Door, StdGen)
reveal g car choice
  | car == choice = (revelation, g')
                    where (revelation, g') = choose g choices
                          choices = [x | x <- [One .. Three], x /= car]

