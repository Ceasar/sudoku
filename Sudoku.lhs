> {-# LANGUAGE FlexibleInstances #-}
> import System.Environment
> import System.Random
> import Data.Char
> import Data.List
> import Data.Maybe
> import qualified Data.Map as M
> import qualified Data.Set as S


The following is heavily based on "Solving Every Sudoku Puzzle" by Peter Norvig.


Notation
========

First we need to define some notation.

A Sudoku puzzle is a grid of 81 squares; the majority of enthusiasts label the
columns 1-9 and the rows A-I.

> type Square = String

> rowLabels = "ABCDEFGHI"
> colLabels = "123456789"

> squares :: S.Set Square
> squares = crossSet rowLabels colLabels

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

A collection of nine squares (column, row, or box) is called a unit.

> cols = S.fromList [crossSet rowLabels [c] | c <- colLabels]
> rows = S.fromList [crossSet [r] colLabels | r <- rowLabels]
> boxs = S.fromList [crossSet rs cs | rs <- ["ABC", "DEF", "GHI"],
>                                     cs <- ["123", "456", "789"]]

> units :: S.Set (S.Set Square)
> units = cols `S.union` rows `S.union` boxs

Squares that share a unit are peers.

> peers :: M.Map Square (S.Set Square)
> peers = M.fromList $ [(s, S.delete s $ S.fold S.union S.empty $ S.filter (S.member s) units) | s <- S.toList squares]

Every square has exactly 3 units and 20 peers.

A puzzle leaves some squares blank and fills others with digits. A puzzle is
solved if the squares in each unit are filled with a permutation of the digits
1 to 9.

> values = S.fromList [1..9]

That is, no digit can appear twice in a unit, and every digit must appear once.
This implies that each square must have a different value from any of its peers.

Representation
==============

Abstractly, we represent a grid as two maps.

The first is a map between Squares and known values.
The second is a map between Squares and the possibilties for the square there.

> type Knowns = M.Map Square Int
> type Unknowns = M.Map Square (S.Set Int)
> data Grid = Grid {knowns :: Knowns, unknowns :: Unknowns} deriving Eq

No square is permitted to exist in both maps.

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

To parse a grid, we have to be a little careful. We cannot just read divide the
input into knowns and unknowns-- we need to make sure that we make all
deductions during the process.

(Actually, I'm not sure this is true, but it is how I am doing it now.)

To do so, we start with an empty grid and, one at a time, assign the initial
values, updating the information in our unknown map on each assignment.

> emptyGrid :: Grid
> emptyGrid = Grid M.empty (M.fromList $ zip (S.toList squares) (repeat values))

> initialValues :: String -> Knowns
> initialValues = foldl f M.empty . zip (S.toList squares) . tokenize
>   where
>       f m (s, Just i)  = M.insert s i m
>       f m (_, Nothing) = m

> foldAdjust :: Ord k => (a -> a) -> S.Set k -> M.Map k a -> M.Map k a
> foldAdjust f ks m = foldl (\m' k -> M.adjust f k m') m (S.toList ks)

> elimFromPeers :: Square -> Int -> Unknowns -> Unknowns
> elimFromPeers s i g = foldAdjust (S.delete i) (justLookup s peers) g

> assign :: Square -> Int -> Grid -> Grid
> assign s i (Grid k uk) = Grid (M.insert s i k) (M.delete s $ elimFromPeers s i uk)

> parseGrid :: String -> Grid
> parseGrid = M.foldWithKey assign emptyGrid . initialValues

Solving Puzzles
===============

At this point, we are ready to start solving puzzles.

Constraint Propogation
----------------------

To solve simple puzzles, it is possible just to propogate constraints until
the puzzle is solved.

For instance, one constraint is that if all of the squares in a unit are known
except one, the last one can be deduced.

> simplify :: Grid -> Grid
> simplify g
>   | M.null singletons = g
>   | otherwise = simplify $ M.foldWithKey assign g $ M.map S.findMin singletons
>   where
>       singletons = M.filter ((== 1) . S.size) $ unknowns g

As an example, here is a puzzle and its solution, which was derived strictly
from application of the above rule.

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

And here is a puzzle and its partial solution, also derived from the
propgating the above rule.

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

So, what's next?

We could try to code more sophisticated strategies. For example, the _naked
twins_ strategy looks for two squares in the same unit that both have same
possible two digits. Given [..., ("A5", [2, 6]), ("A6", [2, 6]), ...], we can
conclude that 2 and 6 must be in A5 and A6 (although we don't know which is
where) and we can therefore eliminate 2 and 6 from every other square in the 'A'
row unit.

Coding up strategies like this is a possible route, but it would require
hundreds of lines of code (there are dozens of these strategies), and we'd never
be sure if we could solve _every_ puzzle.

The other route is to _search_ for a solution.

Search
------

Searching involves systematically trying all the possiblities until we hit one
that works. The code for this is less than a dozen lines, but we run another
risk: the program might take forever to run. How can we cope with this?

There are two choices:

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
> hasContradiction = not . M.null . M.filter S.null . unknowns

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

> solutions :: BT a => Eq a => a -> [a]
> solutions x
>   | reject x  = []
>   | accept x  = x : []
>   | otherwise = nub $ concat $ map solutions $ children x

> search :: BT a => Eq a => a -> Maybe a
> search x = case take 1 $ solutions x of
>   [] -> Nothing
>   (y:_) -> Just y

> justLookup :: Ord k => k -> M.Map k a -> a
> justLookup k m = fromJust $ M.lookup k m

Note, when generating children, we actually only need to find one square and try that.
Generating the full list of possible children is unnecessary since we try to assign
every possible value to the square we choose, so one of them must be right.

Also, for some reason, when I uncomment that line, this takes FOREVER to run.

> guesses :: [Square] -> Grid -> [Grid]
> guesses [] _ = []
> guesses ks g@(Grid _ uk) = [assign s i g | i <- S.toList $ justLookup s uk] -- ++ guesses (delete s ks) g
>   where s = minimumBy (cmpSquare uk) ks

---
Note, if we used regular sorting here, we would have to sort before searching.
We use selection sort instead to keep things lazy.

 sSortBy :: Eq a => (a -> a -> Ordering) -> [a] -> [a]
 sSortBy cmp [] = []
 sSortBy cmp xs = x : sSortBy cmp (delete x xs) where x = minimumBy cmp xs

 minRemVal :: Unknowns -> [Square]
 minRemVal uk = sSortBy (cmpSquare uk) (M.keys uk)
---

> cmpSquare :: Unknowns -> Square -> Square -> Ordering
> cmpSquare uk a b = (remVals a) `compare` (remVals b)
>   where
>       remVals s = S.size $ justLookup s uk

> instance BT Grid where
>   reject = hasContradiction
>   accept = isSolved
>   children g = guesses (M.keys $ unknowns g) g

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

For value ordering, we won't do anything special; we'll consider the digits in
numeric order.

Now we're ready to define `solve` in terms of `search`.

> readGrid :: FilePath -> IO Grid
> readGrid f = do
>   s <- readFile f
>   return $ parseGrid s

> solve :: Grid -> IO (Maybe Grid)
> solve g = do
>   return $ search g

> solvePuzzle :: FilePath -> IO ()
> solvePuzzle f = do
>   g <- readGrid f
>   putStrLn $ show g
>   case search g of
>       Nothing -> putStrLn "No solution"
>       Just x -> putStrLn $ show x

While good enough for most puzzles, there are a certain class of puzzles that
the above cannot solve in a short time.

For one, there is a class of problem which can be engineered to take advantage
of the determinism of the algorithm in order to slow it down by carefully
choosing inputs the algorithm will take a look time to find. This can be fixed
by simply making the choice of square non-deterministic.

Even with nondeterminism, Norvig still found some puzzles that were obnoxiously
difficult to solve. I've provided a few inside of `puzzles` for demonstration
purposes.

> main = do
>   args <- getArgs
>   mapM solvePuzzle args

Puzzle Generation
=================

Now we consider how to generate puzzles.

If we were not too picky, to generate a puzzle we might do the following:

1. First generate a random valid grid.
2. Remove values from the grid until we reach the difficulty level we are happy with.

This procedure will guarantee that solution always exists. However, for many
sorts of puzzles, Sudoku included, a puzzle is considered good only if it has
exactly one solution.

To generate unique puzzles, we need modify our algorithm only as follows:

1. First generate a random valid grid.
2. If we reached the desired difficulty level stop.
3. Generate a list of value to remove from the grid.
4. Choose one and check if there are multiple solutions.
    - If so, try the next value. If no next values exist, stop.
    - Otherwise, go to 2.

While the above works, the problem is that it is unclear how long it will take
to check if there are multiple solutions. To find one, we can use our
strategies from above. But to show there aren't two or more for a puzzle
that only has one solution means to exhaust all possibilities.

For some problems, computing all the possibilities is a pain, but doable.
Sudoku, as we already know, is one of those problems where it is not. As
Norvig described above, the time to run a brute force algorithm is
astronomical.

Nevertheless, I am going to try this, and see how it goes.

Generating a Random Grid
------------------------

The basic idea is use a Las Vegas algorithm to generate the grid.

Namely, we will start with an empty grid, choose a bunch of squares to fill in
and then run our solver to complete the grid.

> pick :: [a] -> IO a
> pick xs = do
>   r <- randomRIO (0, length xs - 1)
>   return $ xs !! r

> genGrid :: IO Grid
> genGrid = do
>   s <- pick (S.toList squares)
>   return $ fromJust $ search $ assign s 1 emptyGrid

Kind of hacky, but this works for now.

Removing Values
---------------

> addToPeers :: Square -> Int -> Unknowns -> Unknowns
> addToPeers s i g = foldAdjust (S.insert i) (justLookup s peers) g

> getPossibleValues :: Square -> Knowns -> S.Set Int
> getPossibleValues s k = S.difference values $ S.fromList $ M.elems pmap
>   where
>       pmap = M.filterWithKey (\k' a -> S.member k' (justLookup s peers)) k

> unassign :: Square -> Grid -> Grid
> unassign s (Grid k uk) = Grid (M.delete s k) (addToPeers s i $ M.insert s ps uk)
>   where
>       ps = getPossibleValues s k
>       i = fromJust $ M.lookup s k

> genPuzzle :: Int -> IO Grid
> genPuzzle 0 = genGrid
> genPuzzle i = do
>   g@(Grid k _) <- genPuzzle (i - 1)
>   s <- pick (M.keys k)
>   return $ unassign s g

This works, although the difficult is unknowable. It's quite possible that
there are multiple solutions, such that the puzzles we are creating might

Another attempt:

> newtype Puzzle = Puzzle Grid deriving (Eq, Show)

> instance BT Puzzle where
>   reject = hasMultipleSolutions
>   accept = isHard
>   children (Puzzle g) = [Puzzle $ unassign s g | s <- M.keys (knowns g)]


I assume that if a puzzle has multiple solutions now, it can never be made
to have a unique solution, which means we can use this to reject.

> hasMultipleSolutions :: Puzzle -> Bool
> hasMultipleSolutions (Puzzle g) = length (solutions g) > 1

> isHard :: Puzzle -> Bool
> isHard (Puzzle g) = 60 > (M.size $ knowns g)

> genPuzzle' :: IO Grid
> genPuzzle' = do
>   g <- genGrid
>   case search $ Puzzle g of
>       Nothing -> error "no solutions"
>       Just (Puzzle x) -> return x

This works. Of course, because our definition was unclear.


Appendix
========

> join :: a -> a -> [a]
> join a b = a:b:[]

> cross :: [a] -> [a] -> [[a]]
> cross xs ys = [join x y | x <- xs, y <- ys]

> crossSet :: [a] -> [a] -> S.Set a
> crossSet xs ys = S.fromList $ cross xs ys

> instance Show Grid where
>   show (Grid k uk) = unlines rows
>       where
>           width = 1
>           divider = take (width * 3 + 2) (repeat '-')
>           line = divider ++ "+" ++ divider ++ "+" ++ divider
>           f s = case M.lookup s k of
>               Just v  -> show v
>               Nothing -> "." -- concat $ map show $ fromJust $ M.lookup s uk
>           rows :: [String]
>           rows = map concat $ divide (9 + 9) $ intersperse " " $ [f s | s<-S.toList squares]
>           divide :: Int -> [a] -> [[a]]
>           divide i [] = []
>           divide i xs = case splitAt i xs of
>               (l, r) -> l : divide i r
