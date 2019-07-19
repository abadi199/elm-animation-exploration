module RotationSpeed exposing
    ( RotationSpeed
    , degPerMs
    , degPerS
    , randomGenerator
    , toDegree
    , toDuration
    )

import Degree exposing (Degree)
import Millisecond exposing (Millisecond)
import Random


type RotationSpeed
    = DegPerS Float


degPerMs : Float -> RotationSpeed
degPerMs n =
    DegPerS (n / 1000)


degPerS : Float -> RotationSpeed
degPerS n =
    DegPerS n


randomGenerator : RotationSpeed -> RotationSpeed -> Random.Generator RotationSpeed
randomGenerator (DegPerS low) (DegPerS high) =
    Random.map DegPerS
        (Random.float low high)


toDegree : Millisecond -> RotationSpeed -> Degree
toDegree ms (DegPerS speed) =
    (speed / 100)
        * (ms |> Millisecond.toFloat)
        |> Degree.deg


toDuration : Degree -> RotationSpeed -> Millisecond
toDuration deg (DegPerS speed) =
    deg
        |> Degree.toFloat
        |> (\n -> n * 1000 / abs speed)
        |> round
        |> Millisecond.millisecond
