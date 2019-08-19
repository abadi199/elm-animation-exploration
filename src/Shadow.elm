module Shadow exposing (style)

import Css exposing (Style, property)


style : Bool -> Style
style show =
    if show then
        property "filter" "drop-shadow(0 0 10px rgba(0,0,0,0.5))"

    else
        property "filter" "none"
