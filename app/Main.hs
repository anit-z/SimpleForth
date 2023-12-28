module Main where

import Interpreter
import Parser
import System.IO (hFlush, stdout)

loop :: Stack -> IO ()
loop st = do
  putStr "> "
  hFlush stdout
  str <- getLine
  let cmds = parse str
  res <- interpret st cmds
  case res of
    Left err -> do
      print err
      return ()
    Right st' -> loop st'

main :: IO ()
main = loop []
