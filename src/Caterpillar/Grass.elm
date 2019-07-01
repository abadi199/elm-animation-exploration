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
            5000

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
            , backgroundSize (px windowWidth)
            , backgroundPosition2 (px backgroundOffset) zero
            , height (vh 50)
            , width (vw 100)
            , position absolute
            , bottom (px 0)
            ]
        ]
        [ H.text "grass"
        ]
