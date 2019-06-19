module Slide.Slide01 exposing (view)

import Css exposing (..)
import Easing
import Fill
import Html
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as HA
import Js.Animation as Animation exposing (animation)
import Js.Animation.Options as Options
import Millisecond exposing (millisecond)
import Px exposing (px)


view : Html msg
view =
    let
        slideOutAnimation =
            H.toUnstyled
                >> animation [ Animation.translate { x = px 5000, y = px 0 } ]
                    (Options.default { duration = millisecond 2000 }
                        |> Options.withEasing Easing.easeInOut
                        |> Options.withFill Fill.forwards
                    )
                >> H.fromUnstyled
    in
    H.div
        [ HA.css
            [ displayFlex
            , alignItems center
            , justifyContent center
            , width (vw 100)
            , height (vh 100)
            , backgroundColor (rgba 0 0 0 0.25)
            ]
        ]
        [ H.h1 [] [ H.text "Slide 1" ] |> slideOutAnimation
        ]
