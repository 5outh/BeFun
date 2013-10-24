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
import Data.Monoid
import Control.Monad.State
import Data.Char(chr, ord, isDigit, digitToInt)
import System.Exit(exitWith, ExitCode(..))
import Control.Monad.Free

subt'     = liftF ((OperationF Subt) ())
askNum'   = liftF ((OperationF AskNum) ())
end'      = liftF End

encapsulateF a = liftF ((OperationF a) ())
add' = encapsulateF Add

program :: Program ()
program = do
  add'
  subt'
  askNum'
  end'

showProgram (Free (OperationF a x)) = show a ++ "\n" ++ showProgram x
showProgram (Free End)              = "End"

{-
  STEP 1: Parse Into a BefungeState
  STEP 2: Figure out program logic and parse into a Free OperationF ()
  STEP 3: Interpret program logic and perform necessary IO operations
-}

process :: BefungeState -> Free OperationF ()
process = error "Not Yet Implemented"

interpret :: Free OperationF () -> IO ()
interpret = error "Not Yet Implemented"

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

-- @TODO: Random Direction Handling
setDirection d (BefungeState is xs loc _ m) = Right $ BefungeState is xs loc d m

-- @TODO : Fix Hardcoded Boundaries
moveB (BefungeState is xs loc d m) = Right $ BefungeState is' xs loc' d m
  where is' = mv is d
        loc' = mvPointTorus 40 25 d loc --hardcoded boundaries atm...gonna fix this!

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

dup (BefungeState is (x:xs) loc dir m) = Right $ BefungeState is (x:x:xs) loc dir m
dup (BefungeState _  []     _   _   _) = Left "Error: Empty Stack; cannot duplicate"

swap (BefungeState is (x:y:xs) loc dir m) = Right $ BefungeState is (y:x:xs) loc dir m
swap (BefungeState _  xs        _  _   _) | length xs < 2 = Left "Error: Too few elements in stack; cannot swap"

--Toggle between strMode and NormalMode
strMode (BefungeState is xs loc dir m) = Right $ BefungeState is xs loc dir $
  case m of
    StringMode -> Normal
    Normal     -> StringMode

askASCII (BefungeState is xs loc dir m) = do
  c <- getChar
  return $ Right $ (BefungeState is ((ord c) : xs) loc dir m)

askInt (BefungeState is xs loc dir m) = do
  c <- getChar
  if (isDigit c)
  then return $ Right (BefungeState is ((digitToInt c) : xs) loc dir m)
  else return $ Left "Error : Pulling digit from non-digit char"

--p	A "put" call (a way to store a value for later use). Pop y, x and v, then change the character at the position (x,y) in the program to the character with ASCII value v
put bs@(BefungeState is (y:x:v:xs) loc dir m) = moveTo' swapped loc
  where bs' = moveTo' bs (x, y)
        swapped = setFocus' bs' (toOp (chr v))
        toOp :: Char -> Free OperationF ()
        toOp = undefined
        
--g	A "get" call (a way to retrieve data in storage). Pop y and x, then push ASCII value of the character at that position in the program
get bs@(BefungeState is (y:x:xs) loc dir m) = moveTo' (BefungeState is (op:xs) (x, y) dir m) loc
  where bs' = moveTo' bs (x, y)
        op  = ord $ fromOp (getFocus' bs')
        fromOp :: Free OperationF () -> Char
        fromOp = undefined

endProgram _ = exitWith ExitSuccess