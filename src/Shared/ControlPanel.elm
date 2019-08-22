module Shared.ControlPanel exposing
    ( State
    , initialState
    , showShadow
    , view
    , withShowShadow
    )

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
        }


type alias StateData =
    { showShadow : Maybe Bool
    }


withShowShadow : Bool -> State -> State
withShowShadow value (State state) =
    State { state | showShadow = Just value }


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


showShadow : State -> Maybe Bool
showShadow (State state) =
    state.showShadow
