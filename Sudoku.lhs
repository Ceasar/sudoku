> {-# LANGUAGE FlexibleInstances #-}
> import System.Environment
> import Data.Char
> import Data.Maybe
> import Data.List
> import qualified Data.Map as M


The following is heavily based on "Solving Every Sudoku Puzzle" by Peter Norvig.


Notation
========

First we need to define some notation.

A Sudoku puzzle is a grid of 81 squares; the majority of enthusiasts label the
columns 1-9 and the rows A-I.

> type Square = String

> rowLabels = "ABCDEFGHI"
> colLabels = "123456789"

> join :: a -> a -> [a]
> join a b = a:b:[]

> cross :: [a] -> [a] -> [[a]]
> cross xs ys = [join x y | x <- xs, y <- ys]

> squares :: [Square]
> squares = cross rowLabels colLabels

A collection of nine squares (column, row, or box) is called a unit.

> rows = [cross [r] colLabels | r <- rowLabels]
> cols = [cross rowLabels [c] | c <- colLabels]
> boxes = [cross rs cs | rs <- ["ABC", "DEF", "GHI"],
>                        cs <- ["123", "456", "789"]]

> units :: [[Square]]
> units = cols ++ rows ++ boxes

Squares that share a unit are peers.

> peers :: M.Map Square [Square]
> peers = M.fromList $ [(s, delete s $ foldl union [] $ filter (elem s) units) | s <- squares]

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

> type Possibilty = Either Int [Int]
> type Grid = M.Map Square Possibilty

 instance Show Possibilty where
   show (Possibility  xs) = concat $ map show xs
   show (Left x) = show x

Textually, we represent a grid as a string of characters with 1-9 indicating a
digit and a 0 or a period specifying an empty square. All other characters are
ignored.

> tokenize :: String -> [Maybe Int]
> tokenize [] = []
> tokenize (x:xs)
>   | x == '.' || x == '0' = Nothing : tokenize xs
>   | isDigit x = Just (read [x] :: Int) : tokenize xs
>   | otherwise = tokenize xs

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

Parsing
=======

To parse a grid, we cannot just assign digits to a Left and non-digits to
Right [1..9] since that would produce an inconsistent grid.

Instead, we start with an empty grid and, one at a time, assign it the initial
values ensuring that the grid is in a consistent state after each assignment.

> emptyGrid :: Grid
> emptyGrid = M.fromList $ zip squares (repeat (Right [1..9]))

> initialValues :: String -> M.Map Square Int
> initialValues = foldl f M.empty . zip squares . tokenize
>   where
>       f m (s, Just i)  = M.insert s i m
>       f m (_, Nothing) = m

> elimFromPeers :: Square -> Int -> Grid -> Grid
> elimFromPeers s i g = foldl (\g' k -> M.adjust (either Left (Right . delete i)) k g') g (fromJust $ M.lookup s peers)

> assign :: Square -> Int -> Grid -> Grid
> assign s i g = M.insert s (Left i) (elimFromPeers s i g)

> parseGrid :: String -> Grid
> parseGrid = M.foldWithKey assign emptyGrid . initialValues


Constraint Propogation
======================

To solve simple sudoku puzzles, it is possible just to apply constraints until
the puzzle is solved.

For instance, one constraint is that if all of the squares in a unit are known
except one, the last one can be deduced.

> unknowns :: Grid -> M.Map Square [Int]
> unknowns = snd . M.mapEither id

> simplify :: Grid -> Grid
> simplify g
>   | M.null singletons = g
>   | otherwise = simplify $ M.foldWithKey assign g $ M.map head singletons
>   where
>       singletons = M.filter ((== 1) . length) $ unknowns g

For instance, here is a simple puzzle and its solution, which can be completely
derived from application of the above rule.

. . 3|. 2 .|6 . .       4 8 3|9 2 1|6 5 7
9 . .|3 . 5|. . 1       9 6 7|3 4 5|8 2 1
. . 1|8 . 6|4 . .       2 5 1|8 7 6|4 9 3
-----+-----+-----       -----+-----+-----
. . 8|1 . 2|9 . .       5 4 8|1 3 2|9 7 6
7 . .|. . .|. . 8       7 2 9|5 6 4|1 3 8
. . 6|7 . 8|2 . .       1 3 6|7 9 8|2 4 5
-----+-----+-----       -----+-----+-----
. . 2|6 . 9|5 . .       3 7 2|6 8 9|5 1 4
8 . .|2 . 3|. . 9       8 1 4|2 5 3|7 6 9
. . 5|. 1 .|3 . .       6 9 5|4 1 7|3 8 2

However, as mentioned this does not work for all puzzles. For instance:

. . .|. . .|9 . 7       . . .|. . .|9 . 7
. . .|4 2 .|1 8 .       . . .|4 2 .|1 8 .
. . .|7 . 5|. 2 6       . . .|7 . 5|. 2 6
-----+-----+-----       -----+-----+-----
1 . .|9 . 4|. . .       1 . .|9 . 4|. . .
. 5 .|. . .|. 4 .       . 5 .|. . .|. 4 .
. . .|5 . 7|. . 9       . . .|5 . 7|. . 9
-----+-----+-----       -----+-----+-----
9 2 .|1 . 8|. . .       9 2 6|1 . 8|. . .
. 3 4|. 5 9|. . .       8 3 4|. 5 9|. . .
5 . 7|. . .|. . .       5 1 7|. . .|. . .

We are still a long way away from solving the puzzle. So, what's next?

We could try to code more sophisticated strategies. For example, the _naked
twins_ strategy looks for two squares in the same unit that both have same
possible two digits. Given [..., ("A5", [2, 6]), ("A6", [2,7]), ...], we can
conclude that 2 and 6 must be in A5 and A6 (although we don't know which is
where) and we can therefore eliminate 2 and 6 from every other square in the 'A'
row unit.

Coding up strategies like this is a possible route, but it would require
hundreds of lines of code (there are dozens of these strategies), and we'd never
be sure if we could solve _every_ puzzle.

The other route is to _search_ for a solution: to systematically try all the
possiblities until we hit one that works. The code for this is less than a dozen
lines, but we run another risk: the program might take forever to run. How can
we cope with this? There are two choices.

First, we could try a brute force approach. Suppose we have a very efficient
program that takes only one instruction to evaluate a position, and that we have
access to the next-generation computing technology, let's say a 10GHz processor
with 1024 cores, and let's say we could afford a million of them, and while
we're shopping, let's say we also pick up a time machine and go back 13 billion
years to the origin of the universe and start our program running. We can then
compute that we'd be almost 1% done with this one puzzle by now.

The second choice is to somehow process more than one possiblity per machine
instruction. That seems impossible, but fortunately it is exactly what
constraint propgation does for us. We don't have to try every possiblity because
as soon as we try one, we immediately eliminate many other possibilities. For
example, if we try an assignment and discover a contradiction, then we've
eliminated not just one possiblity, but fully half of the choices we would have
had to make.

> hasContradiction :: Grid -> Bool
> hasContradiction = not . M.null . M.filter null . unknowns

> isSolved :: Grid -> Bool
> isSolved = M.null . unknowns

What is the search algorithm? Simple: first make sure we haven't already found a
solution or contradiction, and if not, choose one unfilled square and consider
all its possible values. One at a time, try assigning the square each value, and
searching from the resulting position.

What Norvig is describing is an instance of a backtracking algorithm, which has three main components.

Formally, a backtracking algorithm needs three parts:

> class BT a where
>   reject :: a -> Bool -- Stop searching if it can be determined a candidate is unsolvable
>   accept :: a -> Bool -- Stop searching if we have found a solution
>   children :: a -> [a] -- Generate a list of children to continue the search

A reject function which always evaluates True will result in a backtracking algorithm that is equivalent to a brute force search.

Any other reject function 

> search :: BT a => a -> Maybe a
> search x
>   | reject x = Nothing
>   | accept x = Just x
>   | otherwise = case catMaybes $ map search (children x) of
>       [] -> Nothing
>       (y:_) -> Just y

> instance BT Grid where
>   reject = hasContradiction
>   accept = isSolved
>   children g = [assign s i g | i <- vals]
>       where
>           (s, vals) = minRemVal $ M.toList $ unknowns g

In other words, we search for a value 'd' such that we can successfully search
for a solution from the result of assigning square 's' to 'd'. If the search
leads to a failed position, go back and consider another value of 'd'.

This is a _recursive_ search, and we call it a "depth-first search" because
we (recursively) consider all possiblity values before we consider a different
value for 's'.

There are two choices we have to make in implementing the search: "variable
ordering" (which square do we try first?) and "value ordering" (which digit do
we try first for the square?).

For variable ordering, we will use a common heuristic called "minimum remaining
values", which means that we choose the square (or one of the squares) with the
minimum number of possible values. Why? Consider we chose a square with 7
possiblities: we'd expect to guess wrong with probability 6/7. If instead we
chose a square with only 2 possibilities, we'd expect to guess wrong with
probability only 1/2. Thus we choose the square with the fewest possibilities
and the best chance of guessing right.

> minRemVal :: [(Square, [Int])] -> (Square, [Int])
> minRemVal = minimumBy (\a b -> (length $ snd a) `compare` (length $ snd b))

For value ordering, we won't do anything special; we'll consider the digits in
numeric order.

Now we're ready to define `solve` in terms of `search`.

> solve :: FilePath -> IO ()
> solve s = do
>   x <- readFile s
>   putStrLn $ showGrid $ parseGrid x
>   let r = search $ parseGrid x in
>       case r of
>           Nothing -> putStrLn "No solution"
>           Just g -> putStrLn $ showGrid g


Printy Printing
===============


> showGrid g = unlines $ concat $ intersperse [line] [
>                                   [ showLine
>                                       [ concat $ intersperse " " $ map (\x -> center width $ showPos x)
>                                           [fromJust $ M.lookup (join r c) g | c <- cs]
>                                       | cs <- ["123", "456", "789"]]
>                                   | r <- rs]
>                               | rs <- ["ABC", "DEF", "GHI"] ]
>      where
>          width = maximum $ map length $ map showPos (M.elems g)
>          divider = take (width * 3 + 2) (repeat '-')
>          line = divider ++ "+" ++ divider ++ "+" ++ divider
>          showLine xs = (concat $ intersperse "|" xs)
>          showPos :: Possibilty -> String
>          showPos (Left i) = show i
>          showPos (Right is) = show is -- "."
>          center :: Int -> String -> String
>          center i s
>               | i > 1 + length s = " " ++ center (i - 2) s ++ " "
>               | i > length s = ' ' : center (i - 1) s
>               | otherwise = s

Command Line
============

> main = do
>   args <- getArgs
>   mapM solve args
