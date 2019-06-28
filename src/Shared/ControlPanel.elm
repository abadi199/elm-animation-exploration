module Shared.ControlPanel exposing (controlPanel)

import Html as H exposing (Html, div)
import Html.Attributes as HA
import Html.Events as HE


type alias Data a =
    { a | showShadow : Bool }


type alias Handler msg =
    { onShowShadowCheck : Bool -> msg }


controlPanel : Data a -> Handler msg -> Html msg
controlPanel data handler =
    div
        [ HA.style "background" "white"
        , HA.style "position" "absolute"
        ]
        [ showShadowCheckBox data handler ]


showShadowCheckBox : Data a -> Handler msg -> Html msg
showShadowCheckBox data handler =
    H.label []
        [ H.input
            [ HA.type_ "checkbox"
            , HE.onCheck handler.onShowShadowCheck
            , HA.checked data.showShadow
            ]
            []
        , H.text "Show Shadow"
        ]
