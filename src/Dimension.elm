module Dimension exposing
    ( Dimension
    , dimension
    , height
    , multiply
    , multiplyHeight
    , multiplyWidth
    , scale
    , setHeight
    , setWidth
    , toCoordinate
    , toElmCss
    , toString
    , view
    , width
    )

import Coordinate exposing (Coordinate, coordinate)
import Css exposing (..)
import Html.Styled as H
import Html.Styled.Attributes as HA
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


setHeight : Px -> Dimension -> Dimension
setHeight w (Dimension dim) =
    Dimension { dim | height = w }


setWidth : Px -> Dimension -> Dimension
setWidth w (Dimension dim) =
    Dimension { dim | width = w }


multiplyHeight : Float -> Dimension -> Dimension
multiplyHeight multiplier (Dimension dim) =
    { dim | height = dim.height |> Px.multiply multiplier } |> Dimension


multiplyWidth : Float -> Dimension -> Dimension
multiplyWidth multiplier (Dimension dim) =
    { dim | width = dim.width |> Px.multiply multiplier } |> Dimension


multiply : Float -> Dimension -> Dimension
multiply =
    scale


scale : Float -> Dimension -> Dimension
scale scaleFactor dim =
    dim
        |> multiplyWidth scaleFactor
        |> multiplyHeight scaleFactor


toCoordinate : Dimension -> Coordinate
toCoordinate (Dimension dim) =
    coordinate { x = dim.width, y = dim.height }


toString : Dimension -> String
toString (Dimension dim) =
    "(width: "
        ++ Px.toString dim.width
        ++ ", height: "
        ++ Px.toString dim.height
        ++ ")"


view : Dimension -> H.Html msg
view dim =
    H.div
        [ HA.css
            [ position absolute
            , bottom (px 0)
            , right (px 0)
            ]
        ]
        [ H.text <| toString dim ]
