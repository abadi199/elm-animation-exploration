module Shared.ControlPanel exposing
    ( controlPanel
    , view
    , withAnimationType
    , withShowShadowCheck
    )

import AnimationType exposing (AnimationType)
import Css exposing (..)
import Html.Styled as H exposing (Html, div)
import Html.Styled.Attributes as HA
import Html.Styled.Events as HE


type alias State a =
    { a | showShadow : Bool, animationType : AnimationType }


type ControlPanel msg
    = ControlPanel (Options msg)


type alias Options msg =
    { onShowShadowCheck : Maybe (Bool -> msg)
    , onAnimationType : Maybe (AnimationType -> msg)
    }


controlPanel : ControlPanel msg
controlPanel =
    ControlPanel
        { onShowShadowCheck = Nothing
        , onAnimationType = Nothing
        }


withShowShadowCheck : (Bool -> msg) -> ControlPanel msg -> ControlPanel msg
withShowShadowCheck handler (ControlPanel options) =
    ControlPanel { options | onShowShadowCheck = Just handler }


withAnimationType : (AnimationType -> msg) -> ControlPanel msg -> ControlPanel msg
withAnimationType handler (ControlPanel options) =
    ControlPanel { options | onAnimationType = Just handler }


view : State a -> ControlPanel msg -> Html msg
view state (ControlPanel handler) =
    div
        [ HA.css
            [ backgroundColor transparent
            , position absolute
            , top zero
            , left zero
            , displayFlex
            , flexDirection column
            ]
        ]
        [ handler.onShowShadowCheck |> Maybe.map (showShadowCheckBox state) |> Maybe.withDefault (H.text "")
        , handler.onAnimationType |> Maybe.map (showAnimationType state) |> Maybe.withDefault (H.text "")
        ]


showAnimationType : State a -> (AnimationType -> msg) -> Html msg
showAnimationType state handler =
    H.label []
        [ H.input
            [ HA.type_ "checkbox"
            , HE.onCheck
                (\checked ->
                    if checked then
                        handler AnimationType.WebAnimation

                    else
                        handler AnimationType.Elm
                )
            , HA.checked
                (case state.animationType of
                    AnimationType.Elm ->
                        False

                    AnimationType.WebAnimation ->
                        True
                )
            ]
            []
        , H.text "Fast Animation"
        ]


showShadowCheckBox : State a -> (Bool -> msg) -> Html msg
showShadowCheckBox state handler =
    H.label []
        [ H.input
            [ HA.type_ "checkbox"
            , HE.onCheck handler
            , HA.checked state.showShadow
            ]
            []
        , H.text "Show Shadow"
        ]
