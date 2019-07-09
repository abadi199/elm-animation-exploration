module PxPerMs exposing (PxPerMs, pxPerMs)

import Millisecond exposing (Millisecond)
import Px exposing (Px)


type PxPerMs
    = PxPerMs Int


pxPerMs : Int -> PxPerMs
pxPerMs =
    PxPerMs
