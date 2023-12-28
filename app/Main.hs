module Main where

import Interpreter
import Parser
import System.IO (hFlush, stdout)

-- loop until quit
loop :: Stack -> IO ()
loop st = do
  -- show prompt
  putStr "> "
  hFlush stdout
  -- get input line
  str <- getLine
  -- parse commands
  let cmds = parse str
  -- interpret
  res <- interpret st cmds
  case res of
    -- fail, quit
    Left err -> do
      print err
      return ()
    -- success, loop use new stack
    Right st' -> loop st'

-- main program
main :: IO ()
-- use loop
main = loop []
