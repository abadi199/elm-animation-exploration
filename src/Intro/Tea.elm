module Intro.Tea exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


main =
    Browser.sandbox { init = 0, update = update, view = view }



-- MODEL


type alias Model =
    Int



-- UPDATE


type Msg
    = Increment
    | Decrement


update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1



-- VIEW


view model =
    div
        [ style "display" "grid"
        , style "width" "100px"
        , style "grid-template-rows" "1fr 1fr 1fr"
        , style "align-items" "center"
        , style "margin" "auto"
        ]
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt model) ]
        , button [ onClick Increment ] [ text "+" ]
        ]