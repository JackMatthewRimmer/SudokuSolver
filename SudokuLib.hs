import Data.List

ans1 = [[5, 3, 4, 6, 7, 8, 9, 1, 2],
        [6, 7, 2, 1, 9, 5, 3, 4, 8],
        [1, 9, 8, 3, 4, 2, 5, 6, 7],
        [8, 5, 9, 7, 6, 1, 4, 2, 3],
        [4, 2, 6, 8, 5, 3, 7, 9, 1],
        [7, 1, 3, 9, 2, 4, 8, 5, 6],
        [9, 6, 1, 5, 3, 7, 2, 8, 4],
        [2, 8, 7, 4, 1, 9, 6, 3, 5],
        [3, 4, 5, 2, 8, 6, 1, 7, 9]]

inp1 = [[5, 3, 0, 0, 7, 0, 0, 0, 0],
        [6, 0, 0, 1, 9, 5, 0, 0, 0],
        [0, 9, 8, 0, 0, 0, 0, 6, 0],
        [8, 0, 0, 0, 6, 0, 0, 0, 3],
        [4, 0, 0, 8, 0, 3, 0, 0, 1],
        [7, 0, 0, 0, 2, 0, 0, 0, 6],
        [0, 6, 0, 0, 0, 0, 2, 8, 0],
        [0, 0, 0, 4, 1, 9, 0, 0, 5],
        [0, 0, 0, 0, 8, 0, 0, 7, 9]]

-- Preforms an and on all the booleans within the list
listConjunction :: [Bool] -> Maybe Bool
listConjunction [] = Nothing 
listConjunction [x] = Just x 
listConjunction (x : xs) = if x == False then Just False else listConjunction xs

-- Returns True if given list contains no duplicate values
isDistinct :: Eq a => [a] -> Bool
isDistinct [] = True
isDistinct xs = if length xs /= length (nub xs) then False else True

-- maps a boolean function over a list of values and returns and result
boolMap :: (a -> Bool) -> [a] -> Maybe Bool
boolMap f xss = listConjunction (map f xss)

-- want to map isDistinct over the 2d lists
validRows :: Eq a => [[a]] -> Maybe Bool
validRows xss = boolMap isDistinct xss

-- can transpose a 2d array to flip the rows to columns
validColumns :: Eq a => [[a]] -> Maybe Bool
validColumns xss = boolMap isDistinct (transpose xss)

