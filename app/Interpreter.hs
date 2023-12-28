-- Interpreter module
module Interpreter where

import Parser

-- Stack type
type Stack = [Int]

-- Error Type
data Error
  = ProgramQuit
  | DivideByZero
  | NoEnoughData

-- show Error message
instance Show Error where
  show ProgramQuit = "Program quits"
  show DivideByZero = "Divide by zero"
  show NoEnoughData = "There is no enough data on stack"

-- interpret a list of commands using a stack
interpret :: Stack -> [Command] -> IO (Either Error Stack)
-- all commands interpreted, done
interpret st [] = return $ Right st
interpret st (cmd : cmds) =
  -- interpret a command
  case interpretCommand st cmd of
    -- success, no message to show
    (Right st', Nothing) -> interpret st' cmds
    -- success, has message to show
    (Right st', Just s) -> do
      -- show message
      putStrLn s
      interpret st' cmds
    -- failure
    (err, _) -> return err

-- interpret a command
interpretCommand :: Stack -> Command -> (Either Error Stack, Maybe String)
-- put number to stack
interpretCommand st (NUM n) = (Right $ n : st, Nothing)
-- add
interpretCommand st ADD = (interpretBinop st (+) False, Nothing)
-- sub
interpretCommand st SUB = (interpretBinop st (-) False, Nothing)
-- mul
interpretCommand st MUL = (interpretBinop st (*) False, Nothing)
-- div
interpretCommand st DIV = (interpretBinop st div True, Nothing)
-- mod
interpretCommand st MOD = (interpretBinop st mod True, Nothing)
-- negate
interpretCommand (x : st) NEG = (Right $ (-x) : st, Nothing)
-- abs
interpretCommand (x : st) ABS = (Right $ abs x : st, Nothing)
-- dup
interpretCommand (x : st) DUP = (Right $ x : x : st, Nothing)
-- swap
interpretCommand (y : x : st) SWAP = (Right $ y : y : st, Nothing)
-- drop
interpretCommand (x : st) DROP = (Right st, Nothing)
-- pop (show top to console)
interpretCommand (x : st) POP = (Right st, Just $ show x)
-- show (show stack to console)
interpretCommand st SHOW = (Right st, Just $ show st)
-- clear (use a new empty stack)
interpretCommand _ CLEAR = (Right [], Nothing)
-- quit (program quit)
interpretCommand _ QUIT = (Left ProgramQuit, Nothing)
-- no enough data on stack, error
interpretCommand _ _ = (Left NoEnoughData, Nothing)

-- interpret binary operator (+, -, *, /, mod)
interpretBinop :: Stack -> (Int -> Int -> Int) -> Bool -> Either Error Stack
interpretBinop (y : x : st) op False = Right $ op x y : st
-- / and mod need to check y
interpretBinop (y : x : st) op True
  | y /= 0 = Right $ op x y : st
  -- y is 0, error
  | otherwise = Left DivideByZero
-- no enough data on stack for binary operation
interpretBinop _ _ _ = Left NoEnoughData
