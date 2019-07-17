module Shared.ControlPanel exposing
    ( State
    , animationType
    , initialState
    , showShadow
    , view
    , withAnimationType
    , withShowShadow
    )

import AnimationType exposing (AnimationType)
import Css exposing (..)
import Html.Styled as H exposing (Html, div)
import Html.Styled.Attributes as HA
import Html.Styled.Events as HE


type State
    = State StateData


initialState : State
initialState =
    State
        { showShadow = Nothing
        , animationType = Nothing
        }


type alias StateData =
    { showShadow : Maybe Bool
    , animationType : Maybe AnimationType
    }


withShowShadow : Bool -> State -> State
withShowShadow value (State state) =
    State { state | showShadow = Just value }


withAnimationType : AnimationType -> State -> State
withAnimationType value (State state) =
    State { state | animationType = Just value }


view : (State -> msg) -> State -> Html msg
view handler (State state) =
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
        [ state.showShadow
            |> Maybe.map (showShadowCheckBox handler (State state))
            |> Maybe.withDefault (H.text "")
        , state.animationType
            |> Maybe.map (showAnimationType handler (State state))
            |> Maybe.withDefault (H.text "")
        ]


showAnimationType : (State -> msg) -> State -> AnimationType -> Html msg
showAnimationType handler (State state) value =
    H.label []
        [ H.input
            [ HA.type_ "checkbox"
            , HE.onCheck
                (\checked ->
                    if checked then
                        handler (State { state | animationType = Just AnimationType.WebAnimation })

                    else
                        handler (State { state | animationType = Just AnimationType.Elm })
                )
            , HA.checked
                (case value of
                    AnimationType.Elm ->
                        False

                    AnimationType.WebAnimation ->
                        True
                )
            ]
            []
        , H.text "Fast Animation"
        ]


showShadowCheckBox : (State -> msg) -> State -> Bool -> Html msg
showShadowCheckBox handler (State state) value =
    H.label []
        [ H.input
            [ HA.type_ "checkbox"
            , HE.onCheck (\newValue -> handler (State { state | showShadow = Just newValue }))
            , HA.checked value
            ]
            []
        , H.text "Show Shadow"
        ]


animationType : State -> Maybe AnimationType
animationType (State state) =
    state.animationType


showShadow : State -> Maybe Bool
showShadow (State state) =
    state.showShadow
