module Caterpillar.Fence exposing (view)

import Css exposing (..)
import Dimension exposing (Dimension)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as HA
import Px
import Time exposing (Posix)


view : { a | fence : String, windowDimension : Dimension, time : Posix } -> Html msg
view { fence, windowDimension, time } =
    let
        loopDuration =
            10000

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
            [ backgroundImage (url fence)
            , backgroundSize (px windowWidth)
            , backgroundPosition2 (px backgroundOffset) zero
            , backgroundRepeat2 repeat noRepeat
            , height (px 150)
            , width (vw 100)
            , position absolute
            , top (vh 42)
            ]
        ]
        []
