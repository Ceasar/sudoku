# sudoku

Sudoku solver and generator.

# Running the program

This requires System.Random and Test.Quickcheck.

```
$ cabal update && cabal install random

ghci
$ :l Sudoku
$ readGrid "puzzles/easy.sudoku"
$ readGrid "puzzles/easy.sudoku" >>= solveGrid
```

# Solving

The solving code is _heavily_ based on Norvig's paper (norvig.com/sudoku.html).

At the end, I decided there was no good reason to implement any logical deduction strategies at all because searching was so fast. However, the `wikipedia.sudoku` puzzle will show that the Solver's determinism is a weak point, and future work might randomize what values for an unknown square, rather than guessing in order from 1 to 9.

# Generation

I managed to make some progress with puzzle generation, but naturally the problem is in NP, since to verify a unique solution exists involves exhaustively checking the state space. Only puzzles with at most ~17 clues removed can be generated within a reasonably quick time.

The obvious way around this limitation is to devise some induction principle `f` such that applying `f` to a grid that has one unique solution produces a new grid that also has one unique solution, but fewer (or equal) clues. The more complicated `f` is, the more complex the puzzle.

I ran into a problem though in just determining _any_ valid `f`. There is some code near the end that loosely shows what I was trying to do: I wanted to say that if a puzzle is spawned from a puzzle with a unique solution and _every_ deduction we can make results in a puzzle known to have a unique solution (a "good" grid), then that grid must have a unique solution too. However, the principle is empirically not quite right, and the result is that either no clues are removed or all of them are. This idea was based on a paper called "Generating Sudoku Puzzles as an Inverse Problem" which I think in retrospect was overly vague.

Regarding what I learned-- I feel like I learned more about Haskell than recursion to be honest.

- I spent a lot of time playing with the code towards the end of the project hoping to find something that would work but without any luck. I read a bunch of cool paper on generation, but none were detailed enough to produce anything useful or otherwise seemed to use only random generation and test, which is not interesting for this class.
- Many hours were also spent building the solver which was in many ways more interesting.
    - I discovered (and repeatedly rediscovered) that the representation of the grid makes a huge impact on how clean the rest of the code comes out. If the code seems at all simple now (if a bit disorganized) I'd argue it is deceptively so.
    - I learned about backtracking algorithms. They're kind of simple I suppose, but it's a useful concept in the idea of search. I also learned what the problems are in searching huge state spaces and how important it is to use constraints.
    - I discovered Haskell has some quirks regarding laziness. Selection sort for instance, did not seem to evaluate lazily. Fortunately, not a huge problem, but it was a real pain figuring out what was going wrong in my program when I had modified the use of `minimum` to use selection sort instead.
