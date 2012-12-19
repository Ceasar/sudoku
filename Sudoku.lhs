> {-# LANGUAGE FlexibleInstances, OverlappingInstances #-}
>
> module Sudoku where
>
> import System.Environment
> import System.Random
> import Data.Char
> import Data.List
> import Data.Maybe
> import qualified Data.Map as M
> import qualified Data.Set as S

> import Test.QuickCheck


The following is heavily based on "Solving Every Sudoku Puzzle" by Peter Norvig.


Notation
========

First we need to define some notation.

A Sudoku puzzle is a grid of 81 squares.

> type Square = String

The majority of enthusiasts label the columns 1-9 and the rows A-I.

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

> propUnits :: Square -> Bool
> propUnits s = S.size (S.filter (S.member s) units) == 3

> propPeers :: Square -> Bool
> propPeers s = S.size (justLookup s peers) == 20

A puzzle leaves some squares blank and fills others with digits.

> type Value = Int

> values :: S.Set Value
> values = S.fromList [1..9]

A puzzle is solved if the squares in each unit are filled with a permutation of the digits 1 to 9.

> isSolved :: Grid -> Bool
> -- isSolved (Grid k _) = all (== values) [lookupKeySet unit k | unit <- S.toList units]
> isSolved (Grid _ uk) = M.null uk -- hack. the above doesn't quite work

> propSolved :: Grid -> Property
> propSolved g = isSolved g ==> M.null $ unknowns g

That is, no digit can appear twice in a unit, and every digit must appear once.

This implies that each square must have a different value from any of its peers.

Representation
==============

Abstractly, we represent a grid as two maps.

The first is a map between Squares and known values.
The second is a map between Squares and the possibilties for the square there.

> type Knowns = M.Map Square Value
> type Unknowns = M.Map Square (S.Set Value)
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

> exclude :: Square -> Grid -> Grid
> exclude s (Grid k uk) = Grid k (foldAdjust (S.delete v) (justLookup s peers) uk)
>   where
>       v = justLookup s k

> assign :: Square -> Value -> Grid -> Grid
> assign s v (Grid k uk) = Grid (M.insert s v k) (M.delete s uk)

> parseGrid :: String -> Grid
> parseGrid = M.foldWithKey (\s v -> exclude s . assign s v) emptyGrid . initialValues

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
>   | otherwise = simplify $ M.foldWithKey (\s v -> exclude s . assign s v) g $ M.map S.findMin singletons
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


What is the search algorithm? Simple: first make sure we haven't already found a
solution or contradiction, and if not, choose one unfilled square and consider
all its possible values. One at a time, try assigning the square each value, and
searching from the resulting position.

What Norvig is describing is an instance of a backtracking algorithm, which has three main components.

Formally, a backtracking algorithm needs three parts:

A reject function which always evaluates True will result in a backtracking algorithm that is equivalent to a brute force search.

> solutions :: Eq a => (a -> Bool) -> (a -> Bool) -> (a -> [a]) -> a -> [a]
> solutions reject accept children root
>   | reject root  = []
>   | accept root  = root : []
>   | otherwise = nub $ concat $ map (solutions reject accept children) $ children root

> search :: Eq a => (a -> Bool) -> (a -> Bool) -> (a -> [a]) -> a -> Maybe a
> search reject accept children root = case take 1 $ solutions reject accept children root of
>   [] -> Nothing
>   (y:_) -> Just y

Note, when generating children, we actually only need to find one square and try that.
Generating the full list of possible children is unnecessary since we try to assign
every possible value to the square we choose, so one of them must be right.

Also, for some reason, when I uncomment that line, this takes FOREVER to run.

> -- Given a list of squares, choose the one with the least possibilities and
> -- assign it all possible values. (One will be right.)
> guesses :: [Square] -> Grid -> [Grid]
> guesses [] _ = []
> guesses ks g@(Grid _ uk) = [assign s i g | i <- S.toList $ justLookup s uk] -- ++ guesses (delete s ks) g
>   where s = minimumBy (cmpSquare uk) ks

> -- Order squares by number of possibilities
> cmpSquare :: Unknowns -> Square -> Square -> Ordering
> cmpSquare uk a b = (remVals a) `compare` (remVals b)
>   where
>       remVals s = S.size $ justLookup s uk

> -- List of grids resulting from assigning all possible values to a square
> subGrids :: Grid -> [Grid]
> subGrids g = guesses (M.keys $ unknowns g) g

---
Note, if we used regular sorting here, we would have to sort before searching.
We use selection sort instead to keep things lazy.

 sSortBy :: Eq a => (a -> a -> Ordering) -> [a] -> [a]
 sSortBy cmp [] = []
 sSortBy cmp xs = x : sSortBy cmp (delete x xs) where x = minimumBy cmp xs

 minRemVal :: Unknowns -> [Square]
 minRemVal uk = sSortBy (cmpSquare uk) (M.keys uk)
---

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

> solutions' g = solutions hasContradiction isSolved subGrids g

> solve :: Grid -> Maybe Grid
> solve g = search hasContradiction isSolved subGrids g

> readGrid :: FilePath -> IO Grid
> readGrid f = do
>   s <- readFile f
>   return $ parseGrid s

> solvePuzzle :: FilePath -> IO ()
> solvePuzzle f = do
>   g <- readGrid f
>   putStrLn $ show g
>   case solve g of
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

> genGrid :: IO Grid
> genGrid = do
>   s <- pickOne (S.toList squares)
>   return $ fromJust $ solve $ assign s 1 emptyGrid -- known to be multiple solutions

Kind of hacky, but this works for now.

Removing Values
---------------

Removing a value from the grid is procedurally pretty simple. We just delete
it from the list of knowns, add it to the unknowns along with all the possible
values it can take on, and update each of its peers to include it.

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

Generation
----------

With that, we are ready to start generating some puzzles!

> genPuzzle :: Int -> IO Grid
> genPuzzle 0 = genGrid
> genPuzzle i = do
>   g@(Grid k _) <- genPuzzle (i - 1)
>   s <- pickOne (M.keys k)
>   return $ unassign s g

Running `genPuzzle 40` produces something like the following:

. 3 . 1 . . 7 8 . 
5 6 . 7 . 9 2 3 . 
. 8 9 2 . 4 . . 6 
. . . 5 . 7 . 6 8 
. 5 . . 9 . 3 . 2 
6 . . . 1 2 . 7 5 
3 4 5 8 . . 6 . . 
8 . . 9 . 3 . 4 1 
. . . . . . 8 2 3

Unfortunately, we are not guaranteed that the puzzle will have a unique
solution.

We can fix this naively, by just generating puzzles until we find one
that has only one solution.

> hasMultipleSolutions :: Grid -> Bool
> hasMultipleSolutions g = length (solutions' g) > 1

> genPuzzle2 :: Int -> IO Grid
> genPuzzle2 0 = genGrid
> genPuzzle2 i = do
>   p <- genPuzzle i
>   if hasMultipleSolutions p then genPuzzle2 i else return p

However, this quickly becomes intractable. At N = 17, it starts taking a few
seconds to complete. N = 20 is unbearable. As dicussed above, to see that a
puzzle has exactly one solution involves testing every way of solving the
puzzle.


At this point, there are a few paths we could try to get around this:

- Decrease time to determine multiple solutions exist:
    - Utilize constraints to reduce the number of possibilities we have to try.
    - Determine some way to tell if a graph will have multiple solutions
      without testing it.
- Find some way to procedurally remove values that ensures that exactly one
  solution is possible.

The first I think doesn't really solve the problem. It will let us increase
the depth that we can reach, and if we can get it down to only 17 clues that
would be good enough. (For less than 17 clues, it is believed that multiple
solutions will always exist.)

The second I consider far more interesting, but it doesn't seem like it will
be easy.

Procedural Generation
=====================

The following is heavily based on: http://www.math.washington.edu/~morrow/mcm/team2306.pdf

Our problem now is to take a grid which is known to have exactly one solution
and remove a value while ensuring that the grid still has exactly one soluton.

For instance, the grid below is known to have one solution.

Can you remove a value and prove it still has one soluton?

1 5 . 2 . 4 7 8 . 
7 8 9 5 1 6 2 3 4 
. 3 4 7 8 9 1 5 6 
. 2 1 4 . 5 8 . 7 
8 4 5 9 7 . 3 6 2 
6 9 7 3 . . 4 1 5 
4 1 3 . . 7 9 . 8 
5 7 2 8 9 . 6 4 1 
9 6 8 1 . 2 5 7 3

This one is easy of course. We can remove A1 without any trouble. The reason
is that we can deduce that is the only place the 1 could go.

What this suggests is that we can take a variety of forward logical deductions,
determine their inverses, and then apply them sequentially to a solved board
to generate a puzzle.

However, with the exception of the single candidate method, the inverse method
is not always proper: one can utilize an inverse method to remove too much
information from the board.

> -- Remove an assigned value from the Grid
> assign' :: Square -> Grid -> Grid
> assign' s (Grid k uk) = Grid (M.delete s k) (M.insert s (S.singleton v) uk)
>   where
>       v = justLookup s k

Every other method can be reduced to removing possiblities only, so  that in
the end, the only method to reveal a value on the board is this method.

> -- Given a known value, add it to the possibilities of all its peers
> exclude' :: Square -> Grid -> Grid
> exclude' s g@(Grid k uk) = Grid k (foldAdjust (S.insert v) ps uk)
>   where
>       v = justLookup s k
>       ps = S.filter (\p -> M.member p uk) (justLookup s peers)

And more...

> instance Ord Grid where
>   compare a b = compare (M.size $ unknowns a) (M.size $ unknowns b)


> -- For now, just apply assign and exclude
> applyForwardMethods :: Grid -> S.Set Grid
> applyForwardMethods g@(Grid k uk) = assigned -- `S.union` excluded
>   where
>       assigned = S.unions [S.map (\v -> assign s v g) (justLookup s uk) | s <- M.keys uk]
>       -- excluded = S.fromList [exclude s g | s <- M.keys k]

> applyBackwardMethods :: Grid -> S.Set Grid
> applyBackwardMethods g@(Grid k uk) = assigned' -- `S.union` excluded'
>   where
>       assigned' = S.map (\s -> assign'  s g) (M.keysSet k)
>       -- excluded' = S.map (\s -> exclude' s g) (M.keysSet k)

> search' :: Grid -> S.Set Grid -> S.Set Grid -> Grid
> search' q@(Grid k uk) gPs gSs = if S.null gCs then q else search' q' gPs' gCs
>   where
>       children = applyBackwardMethods q
>       gCs = S.filter (\c -> (applyForwardMethods c) `S.isSubsetOf` gPs) children
>       q' = S.findMax gCs -- arbitrary choice based on Ord Grid
>       gPs' = applyForwardMethods q' -- known to be good

> startSearch :: Grid -> IO Grid
> startSearch g = do
>   let children = [assign' s g | s <- M.keys $ knowns g]
>   child <- pickOne children
>   return $ search' child (S.singleton g) (S.fromList children)


Appendix
========

> instance Arbitrary Square where
>   arbitrary = elements (S.toList squares)

> instance Arbitrary Knowns where
>   arbitrary = do
>       squares <- listOf arbitrary
>       values  <- vectorOf (length squares) $ elements $ S.toList values
>       return $ M.fromList (zip squares values)

> instance Arbitrary Grid where
>   arbitrary = do
>       k <- arbitrary
>       return $ M.foldWithKey assign emptyGrid k

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

> cross :: [a] -> [a] -> [[a]]
> cross xs ys = [join x y | x <- xs, y <- ys]

> crossSet :: Ord a => [a] -> [a] -> S.Set [a]
> crossSet xs ys = S.fromList $ cross xs ys

> foldAdjust :: Ord k => (a -> a) -> S.Set k -> M.Map k a -> M.Map k a
> foldAdjust f ks m = foldl (\m' k -> M.adjust f k m') m (S.toList ks)

> join :: a -> a -> [a]
> join a b = a:b:[]

> justLookup :: Ord k => k -> M.Map k a -> a
> justLookup k m = fromJust $ M.lookup k m

> lookupKeys :: Ord k => [k] -> M.Map k v -> [v]
> lookupKeys ks m = catMaybes $ foldl (\vs k -> M.lookup k m : vs) [] ks

> lookupKeySet :: Ord k => Ord v => S.Set k -> M.Map k v -> S.Set v
> lookupKeySet ks = S.fromList . lookupKeys (S.toList ks)

> pickOne :: [a] -> IO a
> pickOne xs = do
>   r <- randomRIO (0, length xs - 1)
>   return $ xs !! r
