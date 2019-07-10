module PxPerMs exposing (PxPerMs, pxPerMs, toPx)

import Millisecond exposing (Millisecond)
import Px exposing (Px)


type PxPerMs
    = PxPerMs Float


pxPerMs : Float -> PxPerMs
pxPerMs =
    PxPerMs


toPx : Millisecond -> PxPerMs -> Px
toPx ms (PxPerMs speed) =
    speed
        * (ms |> Millisecond.toFloat)
        |> round
        |> Px.px
