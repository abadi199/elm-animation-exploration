module Dimension exposing
    ( Dimension
    , dimension
    , height
    , toElmCss
    , width
    )

import Css
import Px exposing (Px)


type Dimension
    = Dimension { width : Px, height : Px }


dimension : { width : Px, height : Px } -> Dimension
dimension =
    Dimension


toElmCss : Dimension -> { width : Css.Px, height : Css.Px }
toElmCss (Dimension dim) =
    { width = dim.width |> Px.toElmCss
    , height = dim.height |> Px.toElmCss
    }


height : Dimension -> Px
height (Dimension dim) =
    dim.height


width : Dimension -> Px
width (Dimension dim) =
    dim.width
