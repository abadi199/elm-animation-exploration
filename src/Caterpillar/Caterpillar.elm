module Caterpillar.Caterpillar exposing (view)

import Css exposing (..)
import Dimension exposing (Dimension)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as HA
import Px


caterpillarWidth : Float
caterpillarWidth =
    600


view : { a | caterpillar : String, windowDimension : Dimension } -> Html msg
view { caterpillar, windowDimension } =
    H.div
        [ HA.css
            [ position absolute
            , left
                (windowDimension
                    |> Dimension.width
                    |> Px.divideBy 2
                    |> Px.add (Px.px (Basics.round -caterpillarWidth // 2))
                    |> Px.toElmCss
                )
            , bottom (px 200)
            , width (px caterpillarWidth)
            ]
        ]
        [ H.img
            [ HA.src caterpillar
            , HA.css
                [ width (pct 100)
                ]
            ]
            []
        ]
