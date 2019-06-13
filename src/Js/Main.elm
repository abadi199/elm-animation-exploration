module Js.Main exposing (main)

import Browser
import Color exposing (Color)
import Coordinate exposing (Coordinate, coordinate)
import Count
import Html exposing (Html, div)
import Html.Attributes as HA
import Html.Events as HE
import Js.Animation as Animation
import Json.Decode as JD
import Percentage
import Px exposing (Px, px)
import Random
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


type Model
    = NotReady
    | Ready Data


type alias Data =
    { boxes : List Box }


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
    , Random.generate RandomGeneratorCompleteBoxes (Random.list 1000 randomBoxGenerator)
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type Msg
    = AnimationFinish
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
        AnimationFinish ->
            ( NotReady, Cmd.none )

        RandomGeneratorCompleteBoxes boxes ->
            ( Ready { boxes = boxes }, Cmd.none )


updateReady : Msg -> Data -> ( Model, Cmd Msg )
updateReady msg data =
    case msg of
        AnimationFinish ->
            let
                _ =
                    Debug.log "AnimationFinish" ""
            in
            ( Ready data, Cmd.none )

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
                    |> List.map animatedBox
                )


animatedBox : Box -> Html Msg
animatedBox box =
    Animation.node
        [ Animation.translate { x = px 0, y = px -200 } (second 0.5)
        ]
        [ HE.on "finish" (JD.succeed AnimationFinish) ]
        (viewBox box.coordinate
            box.color
            [ text "B" ]
        )


viewBox : Coordinate -> Color -> List (Html Msg) -> Html Msg
viewBox coord color children =
    let
        x =
            Coordinate.x coord

        y =
            Coordinate.y coord
    in
    div
        [ HA.style "background" (Color.toCssString color)
        , HA.style "width" "50px"
        , HA.style "height" "50px"
        , HA.style "position" "absolute"
        , HA.style "top" (Px.toString y)
        , HA.style "display" "flex"
        , HA.style "justify-content" "center"
        , HA.style "align-items" "center"
        , HA.style "color" "white"
        , HA.style "left" (Px.toString x)
        ]
        children
