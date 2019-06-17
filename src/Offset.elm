module Offset exposing
    ( Offset
    , none
    , offset
    )


type Offset
    = Offset Float
    | NoOffset


offset : Float -> Offset
offset =
    Offset


none : Offset
none =
    NoOffset
