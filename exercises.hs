data List a = Cons a (List a)
            | Nil
              deriving (Show)

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons first rest) = first : (fromList rest)

data Tree a = TreeNode a (Maybe (Tree a)) (Maybe (Tree a))

listLength :: List a -> Int
listLength Nil = 0
listLength (Cons first rest) = succ (listLength rest)

listSum :: List Double -> Double
listSum Nil = 0
listSum (Cons first rest) = first + listSum rest

listMean :: List Double -> Double
listMean list = listSum list / fromIntegral (listLength list)

listAppend :: List a -> a -> List a
listAppend Nil x = (Cons x Nil)
listAppend (Cons first rest) x = (Cons first (listAppend rest x))

listConcat :: List a -> List a -> List a
listConcat Nil list = list
listConcat list Nil = list
listConcat list (Cons first rest) = listConcat (listAppend list first) rest

listReverse :: List a -> List a
listReverse Nil = Nil
listReverse (Cons first rest) = listAppend (listReverse rest) first

listPalindrome :: List a -> List a
listPalindrome list = listConcat list (listReverse list)

listEquals :: Eq a => List a -> List a -> Bool
listEquals Nil Nil = True
listEquals Nil _ = False
listEquals _ Nil = False
listEquals (Cons first1 rest1) (Cons first2 rest2) = first1 == first2 && listEquals rest1 rest2

isPalindrome :: Eq a => List a -> Bool
isPalindrome list = listEquals list (listReverse list)

intersperse :: a -> [[a]] -> [a]
intersperse sep [] = []
intersperse sep (x:[]) = x
intersperse sep (x:xs) = x ++ [sep] ++ intersperse sep xs

data Direction = Clockwise
               | Counterclockwise
               | Straight
                 deriving (Show)

data Point = Point Double Double
             deriving (Show)

turnDirection :: Point -> Point -> Point -> Direction
turnDirection a b c = undefined
