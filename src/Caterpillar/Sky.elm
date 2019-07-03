module Caterpillar.Sky exposing (view)

import Css exposing (..)
import Dimension exposing (Dimension)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as HA


view : { a | sky : String, windowDimension : Dimension } -> Html msg
view { sky, windowDimension } =
    H.div
        [ HA.css
            [ backgroundImage (url sky)
            , backgroundRepeat repeatX
            , backgroundPosition2 (px 200) zero
            , height (vh 50)
            , width (vw 100)
            ]
        ]
        []
