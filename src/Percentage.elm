module Percentage exposing
    ( Percentage
    , fromSecond
    , toString
    )

import Second exposing (Second)


type Percentage
    = Percentage { numerator : Float, denominator : Float }


fromSecond : { numerator : Second, denominator : Second } -> Percentage
fromSecond { numerator, denominator } =
    Percentage
        { numerator = Second.toFloat numerator
        , denominator = Second.toFloat denominator
        }


toString : Percentage -> String
toString (Percentage { numerator, denominator }) =
    ((numerator / denominator) * 100)
        |> round
        |> String.fromInt
        |> (\pct -> pct ++ "%")
