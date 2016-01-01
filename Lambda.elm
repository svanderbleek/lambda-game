module Lambda where

import Html
import Html.Attributes
import Color
import Hex

type Expression t
  = Hole
  | Variable t
  | Abstraction t (Expression t)
  | Application (Expression t) (Expression t)

coloredBox : Color.Color -> Html.Attribute
coloredBox c =
  Html.Attributes.style [
    ("backgroundColor", Hex.coreColorToCssString c),
    ("display", "inline-block"),
    ("padding", ".4em")
  ]

view : Expression Color.Color -> Html.Html
view e =
  case e of
    Hole -> Html.span [coloredBox Color.blue] []
    Variable t -> Html.span [coloredBox t] []
    Abstraction t te -> Html.span [] [
      Html.text "λ",
      Html.span [coloredBox t] [],
      Html.text ".",
      view te
    ]
    Application e1 e2 -> Html.span [] [
      Html.text "(",
      view e1,
      Html.text " ",
      view e2,
      Html.text ")"
    ]
