module Caterpillar.Grass exposing (view)

import Css exposing (..)
import Dimension exposing (Dimension)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as HA
import Px
import Time exposing (Posix)


view : { a | grass : String, windowDimension : Dimension, time : Posix } -> Html msg
view { grass, windowDimension, time } =
    let
        loopDuration =
            2000

        windowWidth =
            windowDimension
                |> Dimension.width
                |> Px.toInt
                |> toFloat

        tick =
            time
                |> Time.posixToMillis
                |> modBy loopDuration
                |> toFloat

        backgroundOffset =
            (tick * windowWidth) / loopDuration |> negate
    in
    H.div
        [ HA.css
            [ backgroundImage (url grass)
            , backgroundPosition2 (px backgroundOffset) zero
            , height (vh 60)
            , width (vw 100)
            , backgroundSize contain
            , position absolute
            , bottom (px 0)
            ]
        ]
        [ H.text <| String.fromFloat <| backgroundOffset
        ]
