-- Parser module
module Parser where

import Data.Maybe (listToMaybe)

-- Command for Simple Forth
data Command
  = NUM Int     -- put a integer to stack
  | ADD         -- +
  | SUB         -- -
  | MUL         -- *
  | DIV         -- /
  | MOD         -- mod
  | NEG         -- negate
  | ABS         -- abs
  | DUP         -- dup
  | SWAP        -- swap
  | DROP        -- drop
  | POP         -- .
  | SHOW        -- .s
  | QUIT        -- quit
  | CLEAR       -- any other input
  deriving (Show)

-- parse string to int
parseInt :: String -> Maybe Int
parseInt = fmap fst . listToMaybe . reads

-- parse an input string to a list of command
parse :: String -> [Command]
-- call parse' helper function
parse str = parse' (words str) []

-- helper function for parse
parse' :: [String] -> [Command] -> [Command]
-- all done, return parsed input (need reverse)
parse' [] res = reverse res
-- +
parse' ("+" : toks) res = parse' toks (ADD : res)
-- -
parse' ("-" : toks) res = parse' toks (SUB : res)
-- *
parse' ("*" : toks) res = parse' toks (MUL : res)
-- /
parse' ("/" : toks) res = parse' toks (DIV : res)
-- mod
parse' ("mod" : toks) res = parse' toks (MOD : res)
-- negate
parse' ("negate" : toks) res = parse' toks (NEG : res)
-- abs
parse' ("abs" : toks) res = parse' toks (ABS : res)
-- dup
parse' ("dup" : toks) res = parse' toks (DUP : res)
-- swap
parse' ("swap" : toks) res = parse' toks (SWAP : res)
-- drop
parse' ("drop" : toks) res = parse' toks (DROP : res)
-- pop
parse' ("." : toks) res = parse' toks (POP : res)
-- show
parse' (".s" : toks) res = parse' toks (SHOW : res)
-- quit
parse' ("quit" : toks) res = parse' toks (QUIT : res)
-- other input
parse' (tok : toks) res =
  case parseInt tok of
    -- it's an integer
    Just n -> parse' toks (NUM n : res)
    -- not integer, treat as clear stack
    _ -> parse' toks (CLEAR : res)
