module Coordinate exposing
    ( Coordinate
    , add
    , coordinate
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
