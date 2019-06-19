module Slide.Main exposing (main)

import Browser
import Color exposing (Color)
import Coordinate exposing (Coordinate, coordinate)
import Count
import Css exposing (..)
import Degree exposing (deg)
import Dimension exposing (Dimension, dimension)
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
    { slide01 : Slide01.State
    , slide : SlideState
    }


type Slide
    = Slide01


type SlideState
    = Showing Slide
    | Transitioning Slide


init : () -> ( Model, Cmd Msg )
init flags =
    ( Ready { slide01 = Slide01.initialState, slide = Showing Slide01 }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MSG


type Msg
    = AnimationFinish
    | Slide01Msg Slide01Msg


type Slide01Msg
    = Slide01UpdateState Slide01.State
    | Slide01Transition
    | Slide01TransitionFinish



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

        Slide01Msg slide01Msg ->
            updateSlide01 slide01Msg data


updateSlide01 : Slide01Msg -> Data -> ( Model, Cmd Msg )
updateSlide01 msg data =
    case msg of
        Slide01UpdateState state ->
            ( Ready { data | slide01 = state }, Cmd.none )

        Slide01Transition ->
            ( Ready data, Cmd.none )

        Slide01TransitionFinish ->
            ( Ready data, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    H.div
        [ HA.css
            [ fontFamilies [ "Verdana", "Arial" ]
            , displayFlex
            , Css.width (vw 100)
            , Css.height (vh 100)
            , alignItems center
            , justifyContent center
            ]
        ]
        (case model of
            NotReady ->
                [ H.text "Loading..." ]

            Ready data ->
                [ Slide01.view
                    { dimension = dimension { width = px 1000, height = px 800 }
                    , onStateUpdate = Slide01UpdateState
                    , onTransition = Slide01Transition
                    , onTransitionFinish = Slide01TransitionFinish
                    }
                    data.slide01
                    |> H.map Slide01Msg
                ]
        )
