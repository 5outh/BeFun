module BefungeParser(
  operator
)

where

import FunctionProcesses
import Types

import Text.Parsec
import Text.Parsec.String
import System.Random
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans

operator :: GenParser Char st Operation
operator = do
  c <- oneOf "0123456789+-*/%!`><^v?_|\":\\$.,#pg&~@ "
  return $ charToOp c
  
line :: GenParser Char st [Operation]
line = do
  cs <- many1 operator
  optional $ char '\r'
  optional newline
  return $ take 80 cs
  
instrs :: GenParser Char st [[Operation]]
instrs = do
  ops <- many1 line
  return ops
  
--parseBefunge :: String -> Either ParseError (Zipper2D Operation)
parseBefungeInstructions s = case (parse instrs "unknown" s) of
  Right ops -> Torus (mkZipper2DBounded 80 25 ops) 80 25
  Left  e   -> error $ show e
 
test = do
  b <- readFile "../befunge_test.bf"
  let x = parseBefungeInstructions b 
  putStrLn (showInstructions x) 
  return ()