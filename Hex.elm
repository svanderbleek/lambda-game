module Hex where

import Color
import List.Extra
import String

type Value 
  = Hex0
  | Hex1
  | Hex2
  | Hex3
  | Hex4
  | Hex5
  | Hex6
  | Hex7
  | Hex8
  | Hex9 
  | HexA 
  | HexB 
  | HexC 
  | HexD 
  | HexE 
  | HexF 

valueToChar : Value -> Char
valueToChar v =
  case v of
    Hex0 -> '0'
    Hex1 -> '1'
    Hex2 -> '2'
    Hex3 -> '3'
    Hex4 -> '4'
    Hex5 -> '5'
    Hex6 -> '6'
    Hex7 -> '7'
    Hex8 -> '8'
    Hex9 -> '9'
    HexA -> 'A'
    HexB -> 'B'
    HexC -> 'C'
    HexD -> 'D'
    HexE -> 'E'
    HexF -> 'F'

valueToString : Value -> List Char
valueToString v = valueToChar v :: []

valueToInt : Value -> Int
valueToInt v =
  case v of
    Hex0 -> 0
    Hex1 -> 1
    Hex2 -> 2
    Hex3 -> 3
    Hex4 -> 4
    Hex5 -> 5
    Hex6 -> 6
    Hex7 -> 7
    Hex8 -> 8
    Hex9 -> 9
    HexA -> 10
    HexB -> 11
    HexC -> 12
    HexD -> 13
    HexE -> 14
    HexF -> 15

type alias RgbColor r = {r | red: Int, green: Int, blue: Int} 
type ColorComponent = CC Value Value
type Color = C ColorComponent ColorComponent ColorComponent

coreColorToCssString : Color.Color -> String
coreColorToCssString c = "#" ++ coreColorToString c

coreColorToString : Color.Color -> String
coreColorToString c = Color.toRgb c |> rgbColorToColor |> colorToString |> String.fromList

rgbColorToColor : RgbColor r -> Color
rgbColorToColor {red, green, blue} =
  C 
    (intToColorComponent red) 
    (intToColorComponent green) 
    (intToColorComponent blue)

intToColorComponent : Int -> ColorComponent
intToColorComponent i = 
  CC (intToValue i 1) (intToValue i 0)

intToValue : Int -> Int -> Value
intToValue i p = 
  let
    vals = 
      [HexF, HexE, HexD, HexC, HexB, HexA, Hex9, Hex8, Hex7, Hex6, Hex5, Hex4, Hex3, Hex2, Hex1]
  in List.Extra.find (subtractsValue i p) vals |> Maybe.withDefault Hex0

subtractsValue : Int -> Int -> Value -> Bool
subtractsValue i p v = i - (valueToInt v) * 16^p >= 0

colorToString : Color -> List Char
colorToString (C c1 c2 c3) = List.concatMap colorComponentToString [c1, c2, c3]

colorComponentToString : ColorComponent -> List Char
colorComponentToString (CC v1 v2) = List.concatMap valueToString [v1, v2]
