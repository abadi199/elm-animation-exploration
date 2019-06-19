module Slide.Main exposing (main)

import Browser
import Color exposing (Color)
import Coordinate exposing (Coordinate, coordinate)
import Count
import Degree exposing (deg)
import Easing
import Fill
import Html
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as HA
import Html.Styled.Events as HE
import Js.Animation as Animation exposing (animation)
import Js.Animation.Options as Options
import Json.Decode as JD
import Millisecond exposing (millisecond)
import Percentage
import Px exposing (Px, px)
import Random
import Second exposing (second)
import Shared.ControlPanel exposing (controlPanel)
import Slide.Slide01 as Slide01
import Svg as S exposing (..)
import Svg.Attributes as SA exposing (..)



-- MAIN


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view >> H.toUnstyled
        }



-- MODEL


type Model
    = NotReady
    | Ready Data


type alias Data =
    {}


type alias Box =
    {}


init : () -> ( Model, Cmd Msg )
init flags =
    ( Ready {}
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MSG


type Msg
    = AnimationFinish



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        NotReady ->
            updateNotReady msg

        Ready data ->
            updateReady msg data


updateNotReady : Msg -> ( Model, Cmd Msg )
updateNotReady msg =
    case msg of
        AnimationFinish ->
            ( NotReady, Cmd.none )


updateReady : Msg -> Data -> ( Model, Cmd Msg )
updateReady msg data =
    case msg of
        AnimationFinish ->
            let
                _ =
                    Debug.log "AnimationFinish" ""
            in
            ( Ready data, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        NotReady ->
            H.div [] [ H.text "Loading..." ]

        Ready data ->
            H.div []
                [ Slide01.view
                ]
