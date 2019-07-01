module Caterpillar.HillFar exposing (view)

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
            40000

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
            , backgroundSize (px windowWidth)
            , backgroundPosition2 (px backgroundOffset) zero
            , backgroundRepeat2 repeat noRepeat
            , height (vh 100)
            , width (vw 100)
            , position absolute
            , top (vh 30)
            ]
        ]
        []
