module Caterpillar.FastObject exposing (view, viewWithChildren)

import Caterpillar.Shadow as Shadow
import Coordinate exposing (Coordinate)
import Count
import Css exposing (..)
import Dimension exposing (Dimension)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as HA
import Js.Animation as Animation
import Js.Animation.Options as Options
import Millisecond exposing (Millisecond)
import Px
import Speed exposing (Speed)
import Time exposing (Posix)


type alias Options =
    { imageUrl : String
    , name : String
    , windowDimension : Dimension
    , speed : Speed
    , dimension : Dimension
    , coordinate : Coordinate
    , showShadow : Bool
    , isPaused : Bool
    }


viewWithChildren : Options -> List (Html msg) -> Html msg
viewWithChildren { name, isPaused, imageUrl, windowDimension, speed, dimension, coordinate, showShadow } children =
    let
        loopDuration =
            speed |> Speed.toDuration windowWidth

        windowWidth =
            windowDimension
                |> Dimension.width

        keyframes =
            if Speed.isNegative speed then
                [ Animation.translate { x = Px.px 0, y = Px.px 0 }
                , Animation.translate { x = windowWidth |> Px.map negate, y = Px.px 0 }
                ]

            else
                [ Animation.translate { x = windowWidth |> Px.map negate, y = Px.px 0 }
                , Animation.translate { x = Px.px 0, y = Px.px 0 }
                ]

        scaleFactor =
            (windowDimension |> Dimension.width |> Px.toFloat) / 1920

        objectHeight =
            dimension |> Dimension.height |> Px.scale scaleFactor

        bottomCoordinate =
            coordinate
                |> Coordinate.y
                |> Px.scale scaleFactor
    in
    Animation.styledNode
        keyframes
        (Options.default { duration = loopDuration } |> Options.withIterations Count.infinite)
        [ if isPaused then
            HA.attribute "playback" "pause"

          else
            HA.attribute "playback" "play"
        ]
        (H.div
            [ HA.css
                [ backgroundImage (url imageUrl)
                , backgroundSize (Px.toElmCss windowWidth)
                , backgroundRepeat2 repeat noRepeat
                , height (objectHeight |> Px.toElmCss)
                , width (windowWidth |> Px.multiply 2 |> Px.toElmCss)
                , position absolute
                , bottom (bottomCoordinate |> Px.toElmCss)
                , left (coordinate |> Coordinate.x |> Px.toElmCss)
                , Shadow.style showShadow
                ]
            , HA.attribute "data-name" name
            ]
            children
        )


view : Options -> Html msg
view options =
    viewWithChildren options []
