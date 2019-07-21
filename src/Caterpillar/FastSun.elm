module Caterpillar.FastSun exposing (view)

import Caterpillar.Shadow as Shadow
import Css exposing (..)
import Degree
import Dimension exposing (Dimension)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as HA
import Js.Animation as Animation
import Js.Animation.Options as Options
import Millisecond exposing (Millisecond, millisecond)
import Px
import RotationSpeed exposing (RotationSpeed)
import Time exposing (Posix)


sunDimension : Dimension
sunDimension =
    Dimension.dimension
        { width = Px.px 250
        , height = Px.px 250
        }


type alias Options a =
    { a
        | sunUrl : String
        , sunRaysUrl : String
        , windowDimension : Dimension
        , showShadow : Bool
        , rotationSpeed : RotationSpeed
    }


view : Options a -> Html msg
view { rotationSpeed, sunUrl, sunRaysUrl, windowDimension, showShadow } =
    let
        duration =
            rotationSpeed |> RotationSpeed.toDuration (Degree.deg 360)

        windowWidth =
            windowDimension
                |> Dimension.width
                |> Px.toInt
                |> toFloat

        scaleFactor =
            (windowDimension |> Dimension.width |> Px.toFloat) / 1920

        sunScaledDimension =
            sunDimension
                |> Dimension.scale scaleFactor
    in
    H.div
        [ HA.css
            [ backgroundImage (url sunUrl)
            , backgroundRepeat2 noRepeat noRepeat
            , height (sunScaledDimension |> Dimension.height |> Px.toElmCss)
            , width (sunScaledDimension |> Dimension.width |> Px.toElmCss)
            , position absolute
            , left (vw 80)
            , top (vh 5)
            , backgroundSize contain
            , Shadow.style showShadow
            ]
        ]
        [ Animation.styledNode
            [ Animation.rotate (Degree.deg 0)
            , Animation.rotate (Degree.deg 360)
            ]
            (Options.default { duration = duration })
            []
            (H.div
                [ HA.css
                    [ backgroundImage (url sunRaysUrl)
                    , backgroundRepeat2 noRepeat noRepeat
                    , height (sunScaledDimension |> Dimension.height |> Px.toElmCss)
                    , width (sunScaledDimension |> Dimension.width |> Px.toElmCss)
                    , backgroundSize contain
                    , Shadow.style showShadow
                    ]
                ]
                []
            )
        ]
