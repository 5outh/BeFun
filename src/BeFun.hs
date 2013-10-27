{-# LANGUAGE NoMonomorphismRestriction #-}

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
  runStateT execute (Right bfstate)
  return ()