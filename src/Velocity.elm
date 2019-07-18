module Velocity exposing
    ( Velocity
    , distance
    , duration
    , isNegative
    , pxPerMs
    , pxPerS
    )

import Millisecond exposing (Millisecond)
import Px exposing (Px)


type Velocity
    = PxPerS Float


pxPerS : Float -> Velocity
pxPerS =
    PxPerS


pxPerMs : Float -> Velocity
pxPerMs n =
    PxPerS (n / 1000)


distance : Millisecond -> Velocity -> Px
distance ms (PxPerS speed) =
    (speed / 1000)
        * (ms |> Millisecond.toFloat)
        |> round
        |> Px.px


isNegative : Velocity -> Bool
isNegative (PxPerS velocity) =
    velocity < 0


duration : Px -> Velocity -> Millisecond
duration px (PxPerS speed) =
    px
        |> Px.toFloat
        |> (\n -> n * 1000 / abs speed)
        |> round
        |> Millisecond.millisecond
