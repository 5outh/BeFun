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
    return $ Right $
      BefungeState
      is
      []
      (0,0)
      R
      Normal
      gen

handleArgs ("-i":xs) = case (null xs) of
  False -> let (x:_) = xs in do
    gen <- newStdGen
    let is = parseBefungeInstructions x
    return $ Right $
      BefungeState
      is
      []
      (0,0)
      R
      Normal
      gen
      
handleArgs (_:xs) = return $ Left "Please use the flag `-f` and specify a file to run."
handleArgs []     = return $ Left "Usage: BeFun -f \"source_file_name.bf\""

main = do
  args <- getArgs
  bfstate <- handleArgs args
  runStateT execute bfstate
  return ()