module Types(
  BefungeState(..),
  Operation(..),
  Direction(..),
  Point(..),
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
  charToOp,
  opToChar,
  showZipper,
  showZipper2D,
  showInstructions,
  zmap,
  concatZMap,
  toList,
  sample
)where

import Data.Monoid
import Data.Char
import Data.List
import System.Random

data Direction = L | R | U | D deriving (Show, Eq)

type Point = (Int, Int)

data Torus a = Torus{ t_data :: Zipper2D a, w :: Int, h :: Int } deriving (Show)

type Zipper a = ([a], a, [a]) 
type Zipper2D a = Zipper (Zipper a)

-- @TODO : Figure out actual type of instructions...
data BefungeState = BefungeState
  {
    instructions :: Torus Operation, 
    stack :: [Int],
    arrayLoc :: Point, 
    dir :: Direction,
    mode :: Mode,
    rng :: StdGen
  } deriving Show

   
data Mode = StringMode | Normal deriving (Show, Eq)

--Corresponds to the above definitions
data Operation =
  Number Int
  | Add | Subt | Mult | Div | Mod | Not | GT
  | Dir Direction | RandDir
  | PopRL | PopUD
  | Str
  | Duplicate | Swap
  | PopDiscard | PopOutputInt | PopOutputAscii
  | Skip
  | Put | Get
  | AskNum | AskChar
  | Empty
  | Other Char --string mode
  | End
  | BefungeOps [Operation] -- purely for Monoid instance
  deriving (Show, Eq)

charToOp :: Char -> Operation
charToOp  c | c `elem` ['0'..'9'] =   (Number (digitToInt c))
            | otherwise = case c of
                '+'  ->  Add
                '-'  ->  Subt
                '*'  ->  Mult
                '/'  ->  Div
                '%'  ->  Mod
                '!'  ->  Not
                '`'  ->  Types.GT
                '>'  ->  Dir R
                '<'  ->  Dir L
                '^'  ->  Dir U
                'v'  ->  Dir D
                '?'  ->  RandDir
                '_'  ->  PopRL
                '|'  ->  PopUD
                '\"' ->  Str
                ':'  ->  Duplicate
                '\\' ->  Swap
                '$'  ->  PopDiscard
                '.'  ->  PopOutputInt
                ','  ->  PopOutputAscii
                '#'  ->  Skip
                'p'  ->  Put
                'g'  ->  Get
                '&'  ->  AskNum
                '~'  ->  AskChar
                '@'  ->  End
                ' '  ->  Empty
                _    ->  (Other c)

opToChar :: Operation -> Char
opToChar op = case op of
                Number a       -> intToDigit a
                Add            -> '+'
                Subt           -> '-'
                Mult           -> '*'
                Div            -> '/'
                Mod            -> '%'
                Not            -> '!'
                Types.GT       -> '`'
                Dir R          -> '>'
                Dir L          -> '<'
                Dir U          -> '^'
                Dir D          -> 'v'
                RandDir        -> '?'
                PopRL          -> '_'
                PopUD          -> '|'
                Str            -> '\"'
                Duplicate      -> ':'
                Swap           -> '\\'
                PopDiscard     -> '$'
                PopOutputInt   -> '.'
                PopOutputAscii -> ','
                Skip           -> '#'
                Put            -> 'p'
                Get            -> 'g'
                AskNum         -> '&'
                AskChar        -> '~'
                End            -> '@'
                Empty          -> ' '
                (Other c)      -> c  
                
                
-- Not even a valid monoid instance...
instance Monoid Operation where
  mempty = Empty
  mappend a b = BefungeOps [a, b]

zmap :: (a -> b) -> Zipper a -> Zipper b
zmap f (as, b, cs) = (fmap f as, f b, fmap f cs)

concatZMap f (as, b, cs) = (fmap f as) ++ [f b] ++ (fmap f cs)

toList (as, b, cs) = (reverse as) ++ [b] ++ cs

--showInstructions :: Torus Operation -> String
showInstructions = fmap opToChar . concat . toList . zmap toList . t_data
  
moveTo :: Torus a -> Point -> Torus a
moveTo t@(Torus zipper w h) p@(x, y) = 
  if (length tops == y) && (length lefts == x) then t
     --y coordinate is correct, just find x
     else if (length tops == y) then
      case length lefts < x of
        True -> moveTo (Torus (mv2D R zipper) w h) p
        _    -> moveTo (Torus (mv2D L zipper) w h) p
     --otherwise, get the y coordinate right
     else 
      case length tops < y of
        True -> moveTo (Torus (mv2D D zipper) w h) p
        _    -> moveTo (Torus (mv2D U zipper) w h) p
  where (tops, cur, bots)     = zipper
        (lefts, cur', rights) = cur

moveTo' :: BefungeState -> Point -> BefungeState
moveTo' (BefungeState is xs loc dir m r) p = BefungeState (moveTo is p) xs p dir m r

setFocus :: Torus a -> a -> Torus a
setFocus (Torus (a, (l, c, r), b) w h) x = Torus (a, (l, x, r), b) w h

setFocus' :: BefungeState -> Operation -> BefungeState
setFocus' (BefungeState is xs loc dir m r) op = BefungeState (setFocus is op) xs loc dir m r

getFocus :: Torus a -> a
getFocus (Torus (_, (_, c, _), _) w h) = c

getFocus' :: BefungeState -> Operation
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
mkZipper2DBounded w h xs = if (not $ null zippers)
                           then let (z:zs) = take h zippers in ([], z, zs ++ replicate (h - length zs - 1) (mkEmptyZipper w))
                           else ([], head zippers, replicate h (mkEmptyZipper w) )
  where zippers = fmap (mkZipperBounded w) xs

showZipper (a, b, c) = show $ a ++ [b] ++ c

showZipper2D as = concatMap showZipper as
 
mvPointTorus :: Int -> Int -> Direction -> Point -> Point
mvPointTorus w _ L (x, y) = ((x-1) `mod` w, y)
mvPointTorus w _ R (x, y) = ((x+1) `mod` w, y)
mvPointTorus _ h U (x, y) = (x, (y+1) `mod` h)
mvPointTorus _ h D (x, y) = (x, (y-1) `mod` h)

sample = Torus (mkZipper2DBounded 10 10 [["a", "b"]]) 10 10