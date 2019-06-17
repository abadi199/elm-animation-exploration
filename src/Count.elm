module Count exposing
    ( Count
    , encode
    , infinite
    , once
    , toString
    )

import Json.Encode as JE


type Count
    = Once
    | Infinite
    | Many Float


infinite : Count
infinite =
    Infinite


once : Count
once =
    Once


toString : Count -> String
toString count =
    case count of
        Once ->
            "1"

        Infinite ->
            "infinite"

        Many number ->
            String.fromFloat number


encode : Count -> JE.Value
encode count =
    case count of
        Once ->
            JE.int 1

        Infinite ->
            JE.string "Infinity"

        Many number ->
            JE.float number
