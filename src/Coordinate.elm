module Coordinate exposing
    ( Coordinate
    , add
    , coordinate
    , multiplyX
    , multiplyY
    , setX
    , setY
    , toString
    , x
    , y
    )

import Px exposing (Px)


type Coordinate
    = Coordinate { x : Px, y : Px }


coordinate : { x : Px, y : Px } -> Coordinate
coordinate =
    Coordinate


y : Coordinate -> Px
y (Coordinate coord) =
    coord.y


x : Coordinate -> Px
x (Coordinate coord) =
    coord.x


toString : Coordinate -> String
toString (Coordinate coord) =
    "(" ++ Px.toString coord.x ++ "," ++ Px.toString coord.y ++ ")"


add : Coordinate -> Coordinate -> Coordinate
add (Coordinate a) (Coordinate b) =
    Coordinate
        { x = Px.add a.x b.x
        , y = Px.add a.y b.y
        }


setY : Px -> Coordinate -> Coordinate
setY px (Coordinate coord) =
    Coordinate { coord | y = px }


setX : Px -> Coordinate -> Coordinate
setX px (Coordinate coord) =
    Coordinate { coord | x = px }


multiplyY : Float -> Coordinate -> Coordinate
multiplyY multiplier (Coordinate coord) =
    Coordinate { coord | y = coord.y |> Px.multiply multiplier }


multiplyX : Float -> Coordinate -> Coordinate
multiplyX multiplier (Coordinate coord) =
    Coordinate { coord | x = coord.x |> Px.multiply multiplier }
