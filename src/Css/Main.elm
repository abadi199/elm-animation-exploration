module Css.Main exposing (main)

import Browser
import Css.Animation as Animation exposing (px, second, withAnimation)
import Html exposing (Html, div)
import Html.Attributes as HA
import Svg as S exposing (..)
import Svg.Attributes as SA exposing (..)


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    {}


init : () -> ( Model, Cmd Msg )
init flags =
    ( {}
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type Msg
    = DeleteMe


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DeleteMe ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ div
            |> withAnimation [ Animation.right (px 100) (second 1) ]
                [ HA.style "background" "red"
                , HA.style "width" "50px"
                , HA.style "height" "50px"
                , HA.style "position" "absolute"
                , HA.style "top" "calc(50vh - 25px)"
                , HA.style "left" "calc(50vw - 25px)"
                ]
                []
        ]


dot : { x : Int, y : Int } -> Svg msg
dot { x, y } =
    g [ SA.style <| "transform: translate(" ++ String.fromInt x ++ "px," ++ String.fromInt y ++ "px)" ]
        [ node "style"
            []
            [ text """
@keyframes moveRight {
    0% { transform: translate(0, 0); }
    100% { transform: translate(500px, 0); }
}

@keyframes moveUp {
    0% { transform: translate(500px, 0); }
    100% { transform: translate(500px, -500px); }
}

@keyframes blink {
    0% { opacity: 1; }
    100% { opacity: 0; }
}

@keyframes pulse {
    0% { transform: scale(1); }
    100% { transform: scale(1.2); }
}
"""
            ]
        , g
            [ SA.style """
animation: moveRight 5s linear forwards
    , moveUp 5s 5s linear forwards
    , moveRight 5s 15s linear forwards
    , moveUp 5s 20s linear forwards
"""
            ]
            [ circle
                [ cx "0"
                , cy "0"
                , r "50"
                , fill "red"
                , SA.style """
animation: blink 1s infinite
    , pulse 1s infinite
"""
                ]
                []
            ]
        ]
