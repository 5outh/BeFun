{-# LANGUAGE DeriveFunctor #-}

module Types(
  BefungeState(..),
  BefungeOperation(..),
  Direction(..),
  Torus(..),
  Zipper(..),
  Zipper2D(..),
  mvRight, mvLeft,
  mv2D, mv,
  mkZipper, mkEmptyZipper,
  mkZipperBounded, mkZipper2DBounded,
  mvPointTorus,
  Mode(..),
  moveTo, moveTo',
  getFocus, getFocus',
  setFocus, setFocus',
  Operation(..),
  OperationF(..),
  Program,
  Free(..),
  liftOp,
  charToOp
)where

import Data.Monoid
import Control.Monad.Free
import Data.Char

data Direction = L | R | U | D deriving (Show, Eq)

type Point = (Int, Int)

data Torus a = Torus{ t_data :: Zipper2D a, w :: Int, h :: Int } deriving (Show)

type Zipper a = ([a], a, [a]) 
type Zipper2D a = Zipper (Zipper a)

-- @TODO : Figure out actual type of instructions...
data BefungeState = BefungeState
  {instructions :: Torus (Free OperationF ()), 
   stack :: [Int],
   arrayLoc :: Point, 
   dir :: Direction,
   mode :: Mode} deriving Show

data Mode = StringMode | Normal deriving Show

--Corresponds to the above definitions
data Operation =
  Number Int
  | Add | Subt | Mult | Div | Mod | Not | GT
  | Dir Direction | RandDir
  | PopRight | PopLeft
  | Str
  | Duplicate | Swap
  | PopDiscard | PopOutputInt | PopOutputAscii
  | Skip
  | Put | Get
  | AskNum | AskChar
  | Empty
  | Other Char
  | BefungeOps [Operation] -- purely for Monoid instance
  deriving (Show, Eq)
  
type BefungeOperation = Operation

--operation functor for use with the free monad below
data OperationF next = OperationF Operation next | End deriving (Functor, Show)

-- I can't think of a better way of doing this, so it'll work for now.
-- lifts an Operation to a Free (OperationF a)
liftOp :: Operation -> Free OperationF ()
liftOp a = liftF ((OperationF a) ())

charToOp :: Char -> Free OperationF ()
charToOp c | c `elem` ['0'..'9'] = liftOp (Number (digitToInt c))
           | otherwise = case c of
              '+'  -> liftOp Add
              '-'  -> liftOp Subt
              '*'  -> liftOp Mult
              '/'  -> liftOp Div
              '%'  -> liftOp Mod
              '!'  -> liftOp Not
              '`'  -> liftOp Types.GT
              '>'  -> liftOp (Dir R)
              '<'  -> liftOp (Dir L)
              '^'  -> liftOp (Dir U)
              'v'  -> liftOp (Dir D)
              '?'  -> error "Undefined: Random Direction Operation"
              '_'  -> liftOp PopRight
              '|'  -> liftOp PopLeft
              '\"' -> liftOp Str
              ':'  -> liftOp Duplicate
              '\\' -> liftOp Swap
              '$'  -> liftOp PopDiscard
              '.'  -> liftOp PopOutputInt
              ','  -> liftOp PopOutputAscii
              '#'  -> liftOp Skip
              'p'  -> liftOp Put
              'g'  -> liftOp Get
              '&'  -> liftOp AskNum
              '~'  -> liftOp AskChar
              '@'  -> liftF End
              _   -> liftOp (Other c)

--program spec
type Program = Free OperationF

-- Not even a valid monoid instance...
instance Monoid Operation where
  mempty = Empty
  mappend a b = BefungeOps [a, b]
  
moveTo :: Torus a -> Point -> Torus a
moveTo t@(Torus zipper w h) p@(x, y) = 
  if (length tops == y) && (length lefts == x) then t
     --y coordinate is correct, just find x
     else if (length tops == y) then
      moveTo (Torus (mv2D R zipper) w h) p
     --otherwise, get the y coordinate right
     else moveTo (Torus (mv2D D zipper) w h) p
  where (tops, cur, bots)     = zipper
        (lefts, cur', rights) = cur

moveTo' :: BefungeState -> Point -> BefungeState
moveTo' (BefungeState is xs loc dir m) p = BefungeState (moveTo is p) xs p dir m

setFocus :: Torus a -> a -> Torus a
setFocus (Torus (a, (l, c, r), b) w h) x = Torus (a, (l, x, r), b) w h

setFocus' :: BefungeState -> Free OperationF () -> BefungeState
setFocus' (BefungeState is xs loc dir m) op = BefungeState (setFocus is op) xs loc dir m

getFocus :: Torus a -> a
getFocus (Torus (_, (_, c, _), _) w h) = c

getFocus' :: BefungeState -> Free OperationF ()
getFocus' = getFocus . instructions

--Note that Zippers are Toroidal!
mvRight :: Zipper a -> Zipper a
mvRight (xs, x, (y:ys)) = (x:xs, y, ys)
mvRight (xs, x, [])     = mvRight ([], x, reverse xs)

mvLeft :: Zipper a -> Zipper a
mvLeft  ((x:xs), y, ys) = (xs, x, (y:ys))
mvLeft  ([], x, ys)     = mvLeft (reverse ys, x, [])

mv2D :: Direction -> Zipper2D a -> Zipper2D a

{- Up and Down movements correspond to moving global zipper left or right -}
mv2D D   t          = mvRight t
mv2D U   t          = mvLeft  t

{- Right and Left movements correspond to moving all internal zippers left or right-}
mv2D R  (xs, x, ys) = (fmap mvRight xs, mvRight x, fmap mvRight ys)
mv2D L  (xs, y, ys) = (fmap mvLeft xs , mvLeft y , fmap mvLeft ys )

mv :: Torus a -> Direction -> Torus a
mv (Torus d w h) dir = Torus (mv2D dir d) w h

mkZipper :: [a] -> Zipper a
mkZipper (x:xs) = ([], x, xs)

mkEmptyZipper :: (Monoid a) => Int -> Zipper a
mkEmptyZipper n = ([], mempty, replicate n mempty)

mkZipperBounded :: (Monoid a) => Int -> [a] -> Zipper a
mkZipperBounded n (x:xs)
 | length xs >= n = ([], x, take (n-1) xs)
 | otherwise      = ([], x, xs ++ (replicate ( n - length xs - 1) mempty))

mkZipper2DBounded :: (Monoid a) => Int -> Int -> [[a]] -> Zipper2D a
mkZipper2DBounded w h xs = if length zippers >= h 
                           then let (z:zs) = take h zippers in ([], z, zs)
                           else ([], head zippers, replicate (h - length zippers - 1) (mkEmptyZipper w) )
  where zippers = fmap (mkZipperBounded w) xs

mvPointTorus :: Int -> Int -> Direction -> Point -> Point
mvPointTorus w _ L (x, y) = ((x-1) `mod` w, y)
mvPointTorus w _ R (x, y) = ((x+1) `mod` w, y)
mvPointTorus _ h U (x, y) = (x, (y+1) `mod` h)
mvPointTorus _ h D (x, y) = (x, (y-1) `mod` h)