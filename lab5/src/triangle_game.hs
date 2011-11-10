--
-- FILE: triangle_game.hs
--

--
-- This program finds all solutions of the 15-peg triangle game.
--

import Control.Monad
import Array


----------------------------------------------------------------------
-- Types.
----------------------------------------------------------------------

-- A move is three integers representing the location of the starting peg,
-- the location of the peg to jump over, and the final location.
type Move  = (Int, Int, Int)

-- The board is a list of integers, arranged in ascending order.
type Board = [Int]

-- A board solution is a starting board along with
-- a list of (move, board) pairs in the order that they were made,
-- ending in a board with only one peg.
type Solution = (Board, [(Move, Board)])


----------------------------------------------------------------------
-- Global data structures.
----------------------------------------------------------------------

starting_board :: Board
starting_board = [0..3] ++ [5..14]

moves :: [Move]
moves = [( 0,  1,  3),
         ( 3,  1,  0),
         ( 1,  3,  6),
         ( 6,  3,  1),
         ( 3,  6, 10),
         (10,  6,  3),
         ( 2,  4,  7),
         ( 7,  4,  2),
         ( 4,  7, 11),
         (11,  7,  4),
         ( 5,  8, 12),
         (12,  8,  5),
         ( 0,  2,  5),
         ( 5,  2,  0),
         ( 2,  5,  9),
         ( 9,  5,  2),
         ( 5,  9, 14),
         (14,  9,  5),
         ( 1,  4,  8),
         ( 8,  4,  1),
         ( 4,  8, 13),
         (13,  8,  4),
         ( 3,  7, 12),
         (12,  7,  3),
         ( 3,  4,  5),
         ( 5,  4,  3),
         ( 6,  7,  8),
         ( 8,  7,  6),
         ( 7,  8,  9),
         ( 9,  8,  7),
         (10, 11, 12),
         (12, 11, 10),
         (11, 12, 13),
         (13, 12, 11),
         (12, 13, 14),
         (14, 13, 12)]


----------------------------------------------------------------------
-- Input/output.
----------------------------------------------------------------------

-- Pretty-print the board in a triangle shape, with '.' for unoccupied pegs
-- and the peg number for occupied pegs.
-- 11 lines.
print_board :: Board -> IO ()
print_board brd = 
{- TODO -}


-- Pretty-print a move.
print_move :: Move -> IO ()
print_move (from, over, to) =
   putStrLn $ "Jump a peg from position " ++ show from 
              ++ " over position " ++ show over
              ++ " to position " ++ show to ++ "."


-- Print the tally of ending pegs.
-- 6 lines.
print_ending_pegs :: Array Int Int -> IO ()
{- TODO -}
        

-- Pretty-print a solution.
-- 10 lines.
print_solution :: Solution -> IO ()
{- TODO -}


----------------------------------------------------------------------
-- Solving the game.
----------------------------------------------------------------------

-- Return True if a move is valid on a particular board,
-- otherwise False.
-- 2 lines.
valid_move :: Board -> Move -> Bool
{- TODO -}


-- Make a specific move on a specific board.
-- Assumes that the move is valid on the board.
-- 3 lines.
make_move :: Board -> Move -> Board
{- TODO -}


-- Make all possible moves on a given board.
-- Return (move, board) pairs for the boards resulting from the moves.
-- 4 lines.  Uses monadic style (list monad).
make_moves :: Board -> [(Move, Board)]
{- TODO -}


-- Return True if a board is a solution.
is_solution :: Board -> Bool
{- TODO -}


-- Starting from a board, generate a list of lists of all the move/board
-- pairs that lead to a solution.  The original board is not part of the
-- lists.  8 lines.  Uses monadic style (list monad).
update :: Board -> [[(Move, Board)]]
{- TODO -}


-- Compute all possible game solutions starting from a given board.  
-- 1 line.
all_solutions :: Board -> [Solution]
{- TODO -}


-- Compute the number of solutions which end in each numbered peg.
-- Return an array of the count for each peg.
-- 8 lines.
count_ending_pegs :: [Solution] -> Array Int Int
{- TODO -}


----------------------------------------------------------------------
-- Entry point.
----------------------------------------------------------------------

-- Compute all possible solutions to the triangle game puzzle.
-- Print out the number of solutions, the tally of ending pegs
-- and the first solution in move-by-move detail.
main :: IO ()
main = 
   let sols        = all_solutions starting_board
       nsols       = length sols
       sol1        = head $ sols
       ending_pegs = count_ending_pegs sols
       blank_line  = putStr "\n"
   in
   do putStrLn $ "Total number of solutions: " ++ show nsols
      blank_line
      putStrLn $ "Ending peg counts:"
      print_ending_pegs ending_pegs
      blank_line
      putStrLn "Detailed solution:"
      print_solution sol1




