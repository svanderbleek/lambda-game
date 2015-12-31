module Lambda where

import Html

type Expression t
  = Empty
  | Variable t
  | Abstraction t (Expression t)
  | Application (Expression t) (Expression t)

view : Expression String -> String
view e =
  case e of
    Empty -> "(empty)"
    Variable t -> t
    Abstraction t e -> "λ " ++ t ++ " . " ++ view e
    Application e1 e2 -> "(" ++ view e1 ++ " " ++ view e2 ++ ")"
