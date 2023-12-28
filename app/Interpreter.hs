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

instance Show Error where
  show ProgramQuit = "Program quits"
  show DivideByZero = "Divide by zero"
  show NoEnoughData = "There is no enough data on stack"

interpret :: Stack -> [Command] -> IO (Either Error Stack)
interpret st [] = return $ Right st
interpret st (cmd : cmds) =
  case interpretCommand st cmd of
    (Right st', Nothing) -> interpret st' cmds
    (Right st', Just s) -> do
      putStrLn s
      interpret st' cmds
    (err, _) -> return err

interpretCommand :: Stack -> Command -> (Either Error Stack, Maybe String)
interpretCommand st (NUM n) = (Right $ n : st, Nothing)
interpretCommand st ADD = (interpretBinop st (+) False, Nothing)
interpretCommand st SUB = (interpretBinop st (-) False, Nothing)
interpretCommand st MUL = (interpretBinop st (*) False, Nothing)
interpretCommand st DIV = (interpretBinop st div True, Nothing)
interpretCommand st MOD = (interpretBinop st mod True, Nothing)
interpretCommand (x : st) NEG = (Right $ (-x) : st, Nothing)
interpretCommand (x : st) ABS = (Right $ abs x : st, Nothing)
interpretCommand (x : st) DUP = (Right $ x : x : st, Nothing)
interpretCommand (y : x : st) SWAP = (Right $ y : y : st, Nothing)
interpretCommand (x : st) DROP = (Right st, Nothing)
interpretCommand (x : st) POP = (Right st, Just $ show x)
interpretCommand st SHOW = (Right st, Just $ show st)
interpretCommand _ CLEAR = (Right [], Nothing)
interpretCommand _ QUIT = (Left ProgramQuit, Nothing)
interpretCommand _ _ = (Left NoEnoughData, Nothing)

interpretBinop :: Stack -> (Int -> Int -> Int) -> Bool -> Either Error Stack
interpretBinop (y : x : st) op False = Right $ op x y : st
interpretBinop (y : x : st) op True
  | y /= 0 = Right $ op x y : st
  | otherwise = Left DivideByZero
interpretBinop _ _ _ = Left NoEnoughData