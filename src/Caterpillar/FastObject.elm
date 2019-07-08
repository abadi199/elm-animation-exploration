module Caterpillar.FastObject exposing (view)

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
import Time exposing (Posix)


type alias Options =
    { imageUrl : String
    , windowDimension : Dimension
    , loopDuration : Millisecond
    , dimension : Dimension
    , coordinate : Coordinate
    , showShadow : Bool
    }


view : Options -> Html msg
view { imageUrl, windowDimension, loopDuration, dimension, coordinate, showShadow } =
    let
        windowWidth =
            windowDimension
                |> Dimension.width
    in
    Animation.styledNode
        [ Animation.translate { x = Px.px 0, y = Px.px 0 }
        , Animation.translate { x = windowWidth |> Px.map negate, y = Px.px 0 }
        ]
        (Options.default { duration = loopDuration } |> Options.withIterations Count.infinite)
        []
        (H.div
            [ HA.css
                [ backgroundImage (url imageUrl)
                , backgroundSize (Px.toElmCss windowWidth)
                , backgroundRepeat2 repeat noRepeat
                , height (dimension |> Dimension.height |> Px.toElmCss)
                , width (windowWidth |> Px.multiply 2 |> Px.toElmCss)
                , position absolute
                , top (coordinate |> Coordinate.y |> Px.toElmCss)
                , left (coordinate |> Coordinate.x |> Px.toElmCss)
                , Shadow.style showShadow
                ]
            ]
            []
        )
