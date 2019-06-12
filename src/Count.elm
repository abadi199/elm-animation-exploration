module Count exposing
    ( Count
    , infinite
    , once
    , toString
    )


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
