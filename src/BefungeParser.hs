module BefungeParser(
  operator
)

where

import FunctionProcesses
import Types

import Text.Parsec
import Text.Parsec.String

operator :: GenParser Char st Operation
operator = do
  c <- oneOf "0123456789+-*/%!`><^v?_|\":\\$.,#pg&~@ "
  return $ charToOp c