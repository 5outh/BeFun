module Interpreter(
  execute
)

where

import Types
import FunctionProcesses
import BefungeParser --not necessary
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State

fromEither :: Either String b -> b
fromEither (Right x) = x
fromEither (Left a) = error a

modifyE = modify . (. fromEither) . fix

execute :: StateT (Either String BefungeState) IO ()
execute = do
  bs <- get
  let bs' = fromEither bs
  
  --for debugging
  --lift $ putStrLn $ showInstructions $ instructions bs'
  lift $ print $ stack     bs'
  lift $ print $ arrayLoc  bs'
  lift $ print $ getFocus' bs'
  lift $ mapM_ putStrLn $ showInstructions $ instructions bs'
  
  case (getFocus' bs') of
    Number n -> modifyE (num n)
    Add ->  modifyE add
    Subt -> modifyE subt
    Mult -> modifyE mult
    Div ->  modifyE divide
    Mod ->  modifyE modulo
    Not ->  modifyE not'
    Dir d -> modifyE (setDirection d)
    RandDir -> modifyE setRandomDirection
    PopRL -> modifyE popRL
    PopUD -> modifyE popUD
    Str   -> modifyE strMode
    Duplicate -> modifyE dup
    Swap      -> modifyE swap
    PopDiscard -> modifyE pop
    PopOutputInt -> popInt
    PopOutputAscii -> popAscii
    Skip -> modifyE moveB -- just move ahead to next instruction
    Put -> modifyE putOp
    Get -> modifyE getOp
    AskNum -> askInt
    AskChar -> askAscii
    End -> return ()
    Empty -> modify id
    Other c -> modifyE readChar
  if getFocus' bs' == End then return ()
  else do
    -- modify without `fix`
    modify . (. fromEither) $ moveB
    execute