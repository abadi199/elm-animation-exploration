module Speed exposing
    ( Speed
    , isNegative
    , pxPerMs
    , pxPerS
    , toDistance
    , toDuration
    )

import Millisecond exposing (Millisecond)
import Px exposing (Px)


type Speed
    = PxPerS Float


pxPerS : Float -> Speed
pxPerS =
    PxPerS


pxPerMs : Float -> Speed
pxPerMs n =
    PxPerS (n * 1000)


toDistance : Millisecond -> Speed -> Px
toDistance ms (PxPerS speed) =
    (speed / 1000)
        * (ms |> Millisecond.toFloat)
        |> round
        |> Px.px


isNegative : Speed -> Bool
isNegative (PxPerS velocity) =
    velocity < 0


toDuration : Px -> Speed -> Millisecond
toDuration px (PxPerS speed) =
    px
        |> Px.toFloat
        |> (\n -> n * 1000 / abs speed)
        |> round
        |> Millisecond.millisecond
