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

-- TESTING HOME COMPUTER / SOURCETREE

import Types
import Data.Monoid
import Control.Monad.State
import Data.Char(chr)

{- data BefungeState = BefungeState
  {instructions :: Torus BefungeOperation, 
   stack :: [Int],
   arrayLoc :: Point, 
   dir :: Direction} deriving Show -}
   
num a (BefungeState is xs loc dir m)= Right $ BefungeState is (a:xs) loc dir m

bsPopFunBinary f (BefungeState is (x:y:xs) loc dir m) = Right $ BefungeState is ((f x y):xs) loc dir m
bsPopFunBinary f (BefungeState is xs loc dir m) | length xs < 2 = Left "Error: Attempt to perform binary operation without enough numbers in stack"

bsPopFunUnary f (BefungeState is (x:xs) loc dir m) = Right $ BefungeState is (f x : xs) loc dir m
bsPopFunUnary f (BefungeState _ []      _   _   _) = Left "Error: Attempt to perform unary operation with empty stack" 

add     = bsPopFunBinary (+)
subt    = bsPopFunBinary (-)
mult    = bsPopFunBinary (*)
divide  = bsPopFunBinary (\a b -> if a == 0 then 0 else b `div` a)
modulo  = bsPopFunBinary (\a b -> if a == 0 then 0 else b `mod` a)
gt      = bsPopFunBinary (\a b -> if b > a then 1 else 0)
 
not'    = bsPopFunUnary (\x -> if x == 0 then 1 else 0)

setDirection d (BefungeState is xs loc _ m) = Right $ BefungeState is xs loc d m

popRL bs@(BefungeState is (x:xs) loc dir m) = case x of
        0 -> Right $ BefungeState is xs loc R m
        _ -> Right $ BefungeState is xs loc L m
popRL (BefungeState    _  []      _  _   _) = Left "Error: Empty Stack; cannot perform '_'"
 
popUD bs@(BefungeState is (x:xs) loc dir m) = case x of
  0 -> Right $ BefungeState is xs loc D m
  _ -> Right $ BefungeState is xs loc U m
popUD (BefungeState    _  []     _   _   _) = Left $ "Error: Empty Stack; cannot perform '|'"

pop (BefungeState is (x:xs) loc dir m) = Right $ BefungeState is xs loc dir
pop (BefungeState _ []      _   _  _ ) = Left $ "Error: Empty Stack; cannot pop"

popInt   (BefungeState is (x:xs) loc dir m) = do
  putStr (show x)
  return $ Right (BefungeState is xs loc dir m)

popAscii (BefungeState is (x:xs) loc dir m) = do
  putChar (chr x)
  return $ Right (BefungeState is xs loc dir m)

--Toggle between strMode and NormalMode
strMode (BefungeState is xs loc dir m) = BefungeState is xs loc dir $
  case m of
    StringMode -> Normal
    Normal     -> StringMode