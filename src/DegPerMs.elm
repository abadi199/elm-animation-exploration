module DegPerMs exposing
    ( DegPerMs
    , degPerMs
    , degPerS
    , randomGenerator
    , toDegree
    )

import Degree exposing (Degree)
import Millisecond exposing (Millisecond)
import Random


type DegPerMs
    = DegPerMs Degree Millisecond


degPerMs : Degree -> Millisecond -> DegPerMs
degPerMs =
    DegPerMs


degPerS : Float -> DegPerMs
degPerS deg =
    DegPerMs (Degree.deg deg) (Millisecond.millisecond 1000)


randomGenerator : DegPerMs -> DegPerMs -> Random.Generator DegPerMs
randomGenerator (DegPerMs lowDeg lowMs) (DegPerMs highDeg highMs) =
    Random.map2 DegPerMs
        (Degree.randomGenerator lowDeg highDeg)
        (Millisecond.randomGenerator lowMs highMs)


toDegree : Millisecond -> DegPerMs -> Degree
toDegree time (DegPerMs deg ms) =
    Degree.toFloat deg
        * (Millisecond.toFloat time / Millisecond.toFloat ms)
        |> Degree.deg
