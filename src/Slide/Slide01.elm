module Slide.Slide01 exposing
    ( State
    , initialState
    , view
    )

import Css exposing (..)
import Dimension exposing (Dimension)
import Easing
import Fill
import Html
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as HA
import Html.Styled.Events as HE
import Js.Animation as Animation exposing (animation)
import Js.Animation.Options as Options
import Millisecond exposing (millisecond)
import Px exposing (px)


type State
    = State StateData


initialState =
    State { step = -1 }


type alias StateData =
    { step : Int }


type alias Options msg =
    { dimension : Dimension
    , onStateUpdate : State -> msg
    , onTransition : msg
    , onTransitionFinish : msg
    }


view : Options msg -> State -> Html msg
view options (State state) =
    let
        { dimension, onStateUpdate, onTransition, onTransitionFinish } =
            options

        step =
            state.step

        height =
            Dimension.height dimension

        width =
            Dimension.width dimension
    in
    H.div
        [ HA.css
            [ displayFlex
            , alignItems center
            , justifyContent center
            , Css.width (width |> Px.toElmCss)
            , Css.height (height |> Px.toElmCss)
            , backgroundColor (rgba 0 0 0 0.25)
            , flexDirection column
            , overflow hidden
            ]
        , HE.onClick (State { state | step = state.step + 1 } |> onStateUpdate)
        ]
        [ H.h1 [] [ H.text "Slide 1" ]
        , H.ul []
            [ H.li [] [ H.h2 [] [ H.text "Text 1" ] ] |> slideOutIf (step >= 0) width
            , H.li [] [ H.h2 [] [ H.text "Text 2" ] ] |> slideOutIf (step >= 1) width
            ]
        ]
        |> slideOutIf (step >= 2) (Px.negate width)


slideOut : Px.Px -> Html msg -> Html msg
slideOut distance =
    H.toUnstyled
        >> animation [ Animation.translate { x = distance, y = px 0 } ]
            (Options.default { duration = millisecond 1000 }
                |> Options.withEasing Easing.easeInOut
                |> Options.withFill Fill.forwards
            )
        >> H.fromUnstyled


slideOutIf : Bool -> Px.Px -> Html msg -> Html msg
slideOutIf flag distance =
    if flag then
        slideOut distance

    else
        identity
