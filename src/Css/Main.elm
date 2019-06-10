module Css.Main exposing (main)

import Browser
import Css.Animation as Animation exposing (withAnimation)
import Html exposing (Html, div)
import Html.Attributes as HA
import Html.Events as HE
import Px exposing (px)
import Second exposing (second)
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
    { redBoxStart : Bool }


init : () -> ( Model, Cmd Msg )
init flags =
    ( { redBoxStart = False }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type Msg
    = UserClickRedBox


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserClickRedBox ->
            ( { model | redBoxStart = True }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div []
        [ box "red"
            |> (if model.redBoxStart then
                    withAnimation
                        [ Animation.translate { x = px 0, y = px -200 } (second 0.5)
                        , Animation.translate { x = px -200, y = px -200 } (second 0.5)
                            |> Animation.delay (second 0.5)
                        ]
                        [ HA.style "left" "calc(50vw - 500px)" ]
                        [ text "A" ]

                else
                    Animation.none
                        [ HA.style "left" "calc(50vw - 500px)", HE.onClick UserClickRedBox ]
                        [ text "A" ]
               )
        , box "green"
            |> withAnimation
                [ Animation.translate { x = px 0, y = px -200 } (second 0.5)
                    |> Animation.delay (second 0.25)
                ]
                [ HA.style "left" "calc(50vw - 400px)" ]
                [ text "B" ]
        , box "blue"
            |> withAnimation
                [ Animation.translate { x = px 0, y = px -200 } (second 0.5)
                    |> Animation.delay (second 0.5)
                ]
                [ HA.style "left" "calc(50vw - 300px)" ]
                [ text "C" ]
        ]


box : String -> List (Attribute Msg) -> List (Html Msg) -> Html Msg
box color attributes children =
    div
        (HA.style "background" color
            :: HA.style "width" "50px"
            :: HA.style "height" "50px"
            :: HA.style "position" "absolute"
            :: HA.style "top" "calc(50vh - 25px)"
            :: HA.style "display" "flex"
            :: HA.style "justify-content" "center"
            :: HA.style "align-items" "center"
            :: HA.style "color" "white"
            :: attributes
        )
        children
