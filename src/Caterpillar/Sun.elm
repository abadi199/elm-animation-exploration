module Caterpillar.Sun exposing (view)

import Css exposing (..)
import Dimension exposing (Dimension)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as HA
import Px
import Time exposing (Posix)


view : { a | imageUrl : String, windowDimension : Dimension, time : Posix } -> Html msg
view { imageUrl, windowDimension, time } =
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
            [ backgroundImage (url imageUrl)
            , backgroundRepeat2 noRepeat noRepeat
            , height (px 250)
            , width (px 250)
            , position absolute
            , left (vw 80)
            , top (vh 5)
            , backgroundSize contain
            ]
        ]
        []
