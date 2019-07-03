module Caterpillar.Object exposing (view)

import Coordinate exposing (Coordinate)
import Css exposing (..)
import Dimension exposing (Dimension)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as HA
import Millisecond exposing (Millisecond)
import Px
import Time exposing (Posix)


type alias Options =
    { imageUrl : String
    , windowDimension : Dimension
    , time : Posix
    , loopDuration : Millisecond
    , dimension : Dimension
    , coordinate : Coordinate
    }


view : Options -> Html msg
view { imageUrl, windowDimension, time, loopDuration, dimension, coordinate } =
    let
        windowWidth =
            windowDimension
                |> Dimension.width
                |> Px.toInt
                |> toFloat

        tick =
            time
                |> Time.posixToMillis
                |> modBy (Millisecond.toInt loopDuration)
                |> toFloat

        backgroundOffset =
            (tick * windowWidth) / Millisecond.toFloat loopDuration |> negate
    in
    H.div
        [ HA.css
            [ backgroundImage (url imageUrl)
            , backgroundSize (px windowWidth)
            , backgroundPosition2 (px backgroundOffset) zero
            , backgroundRepeat2 repeat noRepeat
            , height (dimension |> Dimension.height |> Px.toElmCss)
            , width (dimension |> Dimension.width |> Px.toElmCss)
            , position absolute
            , top (coordinate |> Coordinate.y |> Px.toElmCss)
            , left (coordinate |> Coordinate.x |> Px.toElmCss)
            ]
        ]
        []
