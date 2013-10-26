{-# LANGUAGE NoMonomorphismRestriction #-}
{-
  From http://en.wikipedia.org/wiki/Befunge#Befunge-93_instruction_list
  0-9	Push this number on the stack
  +	Addition: Pop a and b, then push a+b
  -	Subtraction: Pop a and b, then push b-a
  *	Multiplication: Pop a and b, then push a*b
  /	Integer division: Pop a and b, then push b/a, rounded down. If a is zero, ask the user what result they want.[dubious � discuss]
  %	Modulo: Pop a and b, then push the remainder of the integer division of b/a. If a is zero, ask the user what result they want.[dubious � discuss]
  !	Logical NOT: Pop a value. If the value is zero, push 1; otherwise, push zero.
  `	Greater than: Pop a and b, then push 1 if b>a, otherwise zero.
  >	Start moving right
  <	Start moving left
  ^	Start moving up
  v	Start moving down
  ?	Start moving in a random cardinal direction
  _	Pop a value; move right if value=0, left otherwise
  |	Pop a value; move down if value=0, up otherwise
  "	Start string mode: push each character's ASCII value all the way up to the next "
  :	Duplicate value on top of the stack
  \	Swap two values on top of the stack
  $	Pop value from the stack and discard it
  .	Pop value and output as an integer
  ,	Pop value and output as ASCII character
  #	Trampoline: Skip next cell
  p	A "put" call (a way to store a value for later use). Pop y, x and v, then change the character at the position (x,y) in the program to the character with ASCII value v
  g	A "get" call (a way to retrieve data in storage). Pop y and x, then push ASCII value of the character at that position in the program
  &	Ask user for a number and push it
  ~	Ask user for a character and push its ASCII value
  @	End program
   (space)	No-op. Does nothing
-}

import Types
import FunctionProcesses
import BefungeParser
import Interpreter
import Control.Monad.Trans
import Control.Monad.State
import System.Exit
import System.Random
import System.Environment

{-
  Main Idea:
    STEP 1: Parse Into a BefungeState
    STEP 2: Process events on the torus as they come in, modifying as needed in a StateT BefungeState IO ()
-}

handleArgs ("-f":xs) = case (null xs) of
  False -> let (x:_) = xs in do
    b <- readFile x
    gen <- newStdGen
    let is = parseBefungeInstructions b
    return $
      BefungeState
      is
      []
      (0,0)
      R
      Normal
      gen

main = do
  args <- getArgs
  bfstate <- handleArgs args
  putStrLn $ (showInstructions $ instructions bfstate)
  return ()
  
  
