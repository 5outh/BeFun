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
  setFocus, setFocus'
)where

import Data.Monoid

data Direction = L | R | U | D deriving (Show, Eq)

type Point = (Int, Int)

data Torus a = Torus{ t_data :: Zipper2D a, w :: Int, h :: Int } deriving (Show)

type Zipper a = ([a], a, [a]) 
type Zipper2D a = Zipper (Zipper a)

data BefungeState = BefungeState
  {instructions :: Torus BefungeOperation, 
   stack :: [Int],
   arrayLoc :: Point, 
   dir :: Direction,
   mode :: Mode} deriving Show

data Mode = StringMode | Normal deriving Show

--Corresponds to the above definitions
data BefungeOperation = 
  Number Int
  | Add | Subt | Mult | Div | Mod | Not | GT
  | Dir Direction | RandDir
  | PopRight | PopLeft
  | Str String
  | Duplicate | Swap
  | PopDiscard | PopOutputInt | PopOutputAscii
  | Skip
  | Put | Get
  | AskNum | AskChar
  | End 
  | Empty
  | BefungeOps [BefungeOperation] -- purely for Monoid instance
  deriving (Show, Eq)
  
instance Monoid BefungeOperation where
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

setFocus' :: BefungeState -> BefungeOperation -> BefungeState
setFocus' (BefungeState is xs loc dir m) op = BefungeState (setFocus is op) xs loc dir m

getFocus :: Torus a -> a
getFocus (Torus (_, (_, c, _), _) w h) = c

getFocus' :: BefungeState -> BefungeOperation
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