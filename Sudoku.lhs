> import Data.Char
> import Data.Maybe
> import Data.List
> import qualified Data.Map as M


Appendix
========

> without :: Eq a => [a] -> a -> [a]
> without xs x = filter (/= x) xs

> crossWith :: (a -> b -> c) -> [a] -> [b] -> [c]
> crossWith f xs ys = [f x y | x <- xs, y <- ys]

> cross = crossWith (\a b -> a:b:[])


Notation
========

First we have to agree on some notation.

A Sudoku puzzle is a grid of 81 squares; the majority of enthusiasts label the
columns 1-9 and the rows A-I.

> type Square = String

> rows   = "ABCDEFGHI"
> cols   = "123456789"

> squares = cross rows cols

A collection of nine squares (column, row, or box) is called a unit.


> units :: Square -> [[Square]]
> units s = filter (elem s) (col ++ row ++ box)
>       where
>               col = [cross rows [c] | c <- cols]
>               row = [cross [r] cols | r <- rows]
>               box = [cross rs cs | rs <- ["ABC", "DEF", "GHI"],
>                                    cs <- ["123", "456", "789"]]

Squares that share a unit are peers.

> peers :: Square -> [Square]
> peers s = (foldl union [] $ units s) `without` s

Every square has exactly 3 units and 20 peers.

A puzzle leaves some squares blank and fills others with digits. A puzzle is
solved if the squares in each unit are filled with a permutation of the digits
1 to 9.

> values = [1..9]

That is, no digit can appear twice in a unit, and every digit must appear once.
This implies that each square must have a different value from any of its peers.
Here are the names of the squares, a typical puzzle, and the solution to the
puzzle:

 A1 A2 A3| A4 A5 A6| A7 A8 A9    4 . . |. . . |8 . 5     4 1 7 |3 6 9 |8 2 5 
 B1 B2 B3| B4 B5 B6| B7 B8 B9    . 3 . |. . . |. . .     6 3 2 |1 5 8 |9 4 7
 C1 C2 C3| C4 C5 C6| C7 C8 C9    . . . |7 . . |. . .     9 5 8 |7 2 4 |3 1 6 
---------+---------+---------    ------+------+------    ------+------+------
 D1 D2 D3| D4 D5 D6| D7 D8 D9    . 2 . |. . . |. 6 .     8 2 5 |4 3 7 |1 6 9 
 E1 E2 E3| E4 E5 E6| E7 E8 E9    . . . |. 8 . |4 . .     7 9 1 |5 8 6 |4 3 2 
 F1 F2 F3| F4 F5 F6| F7 F8 F9    . . . |. 1 . |. . .     3 4 6 |9 1 2 |7 5 8 
---------+---------+---------    ------+------+------    ------+------+------
 G1 G2 G3| G4 G5 G6| G7 G8 G9    . . . |6 . 3 |. 7 .     2 8 9 |6 4 3 |5 7 1 
 H1 H2 H3| H4 H5 H6| H7 H8 H9    5 . . |2 . . |. . .     5 7 3 |2 9 1 |6 8 4 
 I1 I2 I3| I4 I5 I6| I7 I8 I9    1 . 4 |. . . |. . .     1 6 4 |8 7 5 |2 9 3

Abstractly, we represent a grid as a map between Squares and their possible
values or the lone value if it is known. Why we do not use a 9x9 will become
evident shortly.

> data Possibilty = Unknown [Int] | Known Int
> type Grid = M.Map Square Possibilty

> instance Show Possibilty where
>   show (Unknown xs) = show xs
>   show (Known x) = show x

We represent a Sudoku playing grid in text as a string of characters with
1-9 indicating a digit and a 0 or period specifying an empty square. All other
characters are ignored.

> tokenize :: String -> [Maybe Int]
> tokenize [] = []
> tokenize (x:xs)
>   | x == '.' || x == '0' = Nothing : tokenize xs
>   | isDigit x = Just (read [x] :: Int) : tokenize xs
>   | otherwise = tokenize xs

To build a grid, we cannot just parse a grid and assign the given values to
Knowns and blank values to Unknown [1..9] since that would produce an
inconsistent grid. Rather, we start with an empty grid and assign it the initial
values in order to ensure the the Unknowns are in a consistent state when we
are done.

> emptyGrid :: Grid
> emptyGrid = M.fromList $ zip squares (repeat (Unknown values))

> initialValues :: String -> [(Square, Int)]
> initialValues s = [(s, i) | s <- squares, i <- (catMaybes $ tokenize s)]

> elim :: Int -> Possibilty -> Possibilty
> elim _ (Known x) = Known x
> elim i (Unknown xs) = Unknown (xs `without` i)

> elimFromPeers :: Grid -> Square -> Int -> Grid
> elimFromPeers g s i = foldl (\h k -> M.adjust (elim i) k h) g (peers s)

> assign :: Grid -> (Square, Int) -> Grid
> assign g (s, i) = M.insert s (Known i) (elimFromPeers g s i)

> parseGrid :: String -> Grid
> parseGrid g = foldl assign emptyGrid (initialValues g)

Thus, the following grids are all equivalent:

4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......

400000805
030000000
000700000
020000060
000080400
000010000
000603070
500200000
104000000

4 . . |. . . |8 . 5 
. 3 . |. . . |. . . 
. . . |7 . . |. . . 
------+------+------
. 2 . |. . . |. 6 . 
. . . |. 8 . |4 . . 
. . . |. 1 . |. . . 
------+------+------
. . . |6 . 3 |. 7 . 
5 . . |2 . . |. . . 
1 . 4 |. . . |. . .

Constraint Propogation
======================




> main = do
>   x <- readFile "puzzle1.sudoku"
>   print $ tokenize x




---
 instance Show Grid where
   show (Grid g) = 
       where
           width = maximum $ map length $ map show (M.elems g)
           divider = take width (repeat '-')
           line = divider ++ "+" ++ divider ++ "+" ++ divider
           showLine xs = concat $ intersperse "|" (map show xs)
           line1