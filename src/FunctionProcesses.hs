module FunctionProcesses(
  fix,
  num,
  readChar,
  add, subt, mult, divide, modulo, gt, not',
  setDirection, setRandomDirection,
  moveB, 
  popRL, popUD, pop, popInt, popAscii,
  dup, swap,
  strMode,
  askAscii, askInt,
  putOp, getOp,
  endProgram
)

where

import Types
import Data.Monoid

import Data.Char(chr, ord, isDigit, digitToInt)
import System.Exit(exitWith, ExitCode(..))
import System.Random
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State


-- To handle mode!
fix f bs@(BefungeState _ _ _ _ m _) | m == Normal = f bs
                                    | otherwise = readChar bs

readChar bs@(BefungeState is xs loc dir m r) = let focus = getFocus' bs in
  case focus of
    Str -> Right $ BefungeState is xs loc dir Normal r
    _   -> Right $ BefungeState is ((ord $ opToChar focus):xs) loc dir m r
                                    
bsPopFunBinary f (BefungeState is (x:y:xs) loc dir m r) = Right $ BefungeState is ((f x y):xs) loc dir m r
bsPopFunBinary f (BefungeState is xs loc dir m r) | length xs < 2 = Left "Error: Attempt to perform binary operation without enough numbers in stack"

bsPopFunUnary f (BefungeState is (x:xs) loc dir m r) = Right $ BefungeState is (f x : xs) loc dir m r
bsPopFunUnary f (BefungeState _ []      _   _   _ _) = Left "Error: Attempt to perform unary operation with empty stack" 

add, subt, mult, divide, modulo, gt, not' :: BefungeState -> Either String BefungeState
add     = bsPopFunBinary (+)
subt    = bsPopFunBinary (flip (-))
mult    = bsPopFunBinary (*)
divide  = bsPopFunBinary (\a b -> if a == 0 then 0 else b `div` a)
modulo  = bsPopFunBinary (\a b -> if a == 0 then 0 else b `mod` a)
gt      = bsPopFunBinary (\a b -> if b > a then 1 else 0) 
not'    = bsPopFunUnary (\x -> if x == 0 then 1 else 0)

num :: Int -> BefungeState -> Either String BefungeState
num a (BefungeState is xs loc dir m r)= Right $ BefungeState is (a:xs) loc dir m r

setDirection :: Direction -> BefungeState -> Either String BefungeState
setDirection d (BefungeState is xs loc _ m r) = Right $ BefungeState is xs loc d m r

-- @TODO: Random Direction Handling (Give back a new StdGen)
setRandomDirection :: BefungeState -> Either String BefungeState
setRandomDirection (BefungeState is xs loc _ m gen) = do
  let (val, gen') = randomR (0, 3) gen
      dir = [L,R,U,D] !! val
  return (BefungeState is xs loc dir m gen')

-- @TODO : Fix Hardcoded Boundaries
moveB, popRL, popUD, pop :: BefungeState -> Either String BefungeState
moveB (BefungeState is xs loc d m r) = Right $ BefungeState is' xs loc' d m r
  where is'  = mv is d
        loc' = mvPointTorus 40 25 d loc --hardcoded boundaries atm...gonna fix this!

popRL bs@(BefungeState is (x:xs) loc dir m r) = case x of
        0 -> Right $ BefungeState is xs loc R m r
        _ -> Right $ BefungeState is xs loc L m r
--if empty stack, return 0
popRL bs@(BefungeState is [] loc dir m r) = Right $ BefungeState is [] loc R m r
 
popUD bs@(BefungeState is (x:xs) loc dir m r) = case x of
  0 -> Right $ BefungeState is xs loc D m r
  _ -> Right $ BefungeState is xs loc U m r
--if empty stack, return 0
popUD (BefungeState is [] loc dir m r) = Right $ BefungeState is [] loc D m r

pop (BefungeState is (x:xs) loc dir m r) = Right $ BefungeState is xs loc dir m r
--don't fail on pop, just don't do anything.
pop (BefungeState is []     loc dir m r) = Right $ BefungeState is [] loc dir m r

dup, swap, strMode :: BefungeState -> Either String BefungeState
dup (BefungeState is (x:xs) loc dir m r) = Right $ BefungeState is (x:x:xs) loc dir m r
--don't fail on duplicate, just don't do anything.
dup bs                                   = Right bs

swap (BefungeState is (x:y:xs) loc dir m r) = Right $ BefungeState is (y:x:xs) loc dir m r
swap (BefungeState _  xs        _  _   _ _) | length xs < 2 = Left "Error: Too few elements in stack; cannot swap"

--Toggle between strMode and NormalMode
strMode (BefungeState is xs loc dir m r) = Right $ BefungeState is xs loc dir 
  (case m of
    StringMode -> Normal
    Normal     -> StringMode)
  r

{-
  IO Operations
-}

putEither f = do
  bs <- get
  case bs of
    Right bs' -> put $ f bs' 
    _         -> put $ Left "Error: An error occurred when processing your request"

askAscii, askInt, popInt, popAscii :: StateT (Either String BefungeState) IO ()
askAscii = do
  c <- lift getChar
  bs <- get
  case bs of
    (Right (BefungeState is xs loc dir m r)) ->
      put $ Right $ BefungeState is ((ord c) : xs) loc dir m r
    _ -> put $ Left "Error: Passing an error message into askAscii"

askInt = do
  c <- lift getChar
  bs <- get
  if (isDigit c)
  then case bs of
    Right (BefungeState is xs loc dir m r) -> 
     put $ Right (BefungeState is ((digitToInt c) : xs) loc dir m r)
    _ -> put $ Left "Error: An error occurred while processing your request"
  else put $ Left "Error : Pulling digit from non-digit char"

popInt = do
  bs <- get
  case bs of
    Right (BefungeState is (x:xs) loc dir m r) -> do
      lift $ putStr (show x)
      put $ Right (BefungeState is xs loc dir m r)
    _ -> put $ Left "Error: An error occurred while processing your request"

popAscii = do
  bs <- get
  case bs of
    Right (BefungeState is (x:xs) loc dir m r) -> do
     lift $ putChar (chr x)
     put $ Right (BefungeState is xs loc dir m r)
    _ -> put $ Left "Error: An error occurred while processing your request"
  
--p	A "put" call (a way to store a value for later use). Pop y, x and v, then change the character at the position (x,y) in the program to the character with ASCII value v
putOp, getOp :: BefungeState -> Either String BefungeState
putOp bs@(BefungeState is (y:x:v:xs) loc dir m r) = Right final
  where moved   = moveTo' (BefungeState is xs loc dir m r) (x, y)
        swapped = setFocus' moved (charToOp (chr v))
        final   = moveTo'   swapped loc

--g	A "get" call (a way to retrieve data in storage). Pop y and x, then push ASCII value of the character at that position in the program
getOp bs@(BefungeState is (y:x:xs) loc dir m r) = Right $ BefungeState is (op:xs) loc dir m r
  where moved = moveTo' bs (x, y)
        op    = ord $ opToChar (getFocus' moved)

endProgram _ = exitWith ExitSuccess