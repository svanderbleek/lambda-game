module Lambda where

import Html

type Expression t
  = Hole
  | Variable t
  | Abstraction t (Expression t)
  | Application (Expression t) (Expression t)

view : Expression String -> String
view e =
  case e of
    Hole -> "_"
    Variable t -> t
    Abstraction t e -> "λ " ++ t ++ " . " ++ view e
    Application e1 e2 -> "(" ++ view e1 ++ " " ++ view e2 ++ ")"
