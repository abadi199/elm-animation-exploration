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
            , backgroundRepeat repeat
            , height (vh 100)
            , width (vw 100)
            , position absolute
            , left (px 0)
            , top (px 0)
            ]
        , HA.attribute "data-name" "sky"
        ]
        []
