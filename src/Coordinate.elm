module Coordinate exposing
    ( Coordinate
    , coordinate
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
