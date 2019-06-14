module Elm.Main exposing (main)

import Browser
import Browser.Events
import Color exposing (Color)
import Coordinate exposing (Coordinate, coordinate)
import Count
import Html exposing (Html, div)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD
import Percentage
import Px exposing (Px, px)
import Random
import Second exposing (second)
import Svg as S exposing (..)
import Svg.Attributes as SA exposing (..)
import Time exposing (Posix)


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type Model
    = NotReady
    | Ready Data


type alias Data =
    { boxes : List Box, time : Posix }


type alias Box =
    { coordinate : Coordinate, color : Color }


randomColorGenerator : Random.Generator Color
randomColorGenerator =
    Random.map3 Color.rgb
        (Random.float 0 1)
        (Random.float 0 1)
        (Random.float 0 1)


randomBoxGenerator : Random.Generator Box
randomBoxGenerator =
    Random.map3 (\x y color -> { coordinate = coordinate { x = x, y = y }, color = color })
        (Px.randomGenerator -100 2000)
        (Px.randomGenerator -100 1000)
        randomColorGenerator


init : () -> ( Model, Cmd Msg )
init flags =
    ( NotReady
    , Random.generate RandomGeneratorCompleteBoxes (Random.list 2000 randomBoxGenerator)
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onAnimationFrame AnimationFrameTick


type Msg
    = AnimationFrameTick Posix
    | RandomGeneratorCompleteBoxes (List Box)


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
        AnimationFrameTick _ ->
            ( NotReady, Cmd.none )

        RandomGeneratorCompleteBoxes boxes ->
            ( Ready { boxes = boxes, time = Time.millisToPosix 0 }, Cmd.none )


updateReady : Msg -> Data -> ( Model, Cmd Msg )
updateReady msg data =
    case msg of
        AnimationFrameTick time ->
            ( Ready { data | time = time }, Cmd.none )

        RandomGeneratorCompleteBoxes boxes ->
            ( Ready { data | boxes = boxes }, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        NotReady ->
            div [] [ text "Loading..." ]

        Ready data ->
            div []
                (data.boxes
                    |> List.map (animatedBox data.time)
                )


animatedBox : Posix -> Box -> Html Msg
animatedBox time box =
    viewBox time box [ text "Elm" ]


timeToRotation : Posix -> Int
timeToRotation time =
    let
        millisecond =
            Time.posixToMillis time
    in
    if millisecond == 0 then
        0

    else
        modBy 3600 millisecond // 10


viewBox : Posix -> Box -> List (Html Msg) -> Html Msg
viewBox time box children =
    let
        x =
            Coordinate.x box.coordinate

        y =
            Coordinate.y box.coordinate

        rotation =
            "rotate(" ++ (time |> timeToRotation |> String.fromInt) ++ "deg)"
    in
    div
        [ HA.style "background" (Color.toCssString box.color)
        , HA.style "box-shadow" "0 0 10px rgba(0,0,0,0.5)"
        , HA.style "width" "50px"
        , HA.style "height" "50px"
        , HA.style "position" "absolute"
        , HA.style "top" (Px.toString y)
        , HA.style "display" "flex"
        , HA.style "justify-content" "center"
        , HA.style "align-items" "center"
        , HA.style "color" "white"
        , HA.style "left" (Px.toString x)
        , HA.style "transform" rotation
        ]
        children
