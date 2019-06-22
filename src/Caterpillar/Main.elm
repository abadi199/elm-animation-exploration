module Caterpillar.Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Color exposing (Color)
import Coordinate exposing (Coordinate, coordinate)
import Count
import Dimension exposing (Dimension, dimension)
import Html as H exposing (Html, div)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD
import Percentage
import Px exposing (Px, px)
import Random
import Second exposing (second)
import Shared.ControlPanel exposing (controlPanel)
import Svg as S exposing (..)
import Svg.Attributes as SA exposing (..)
import Task
import Time exposing (Posix)



-- CONSTANTS


constBoxDimension : Dimension
constBoxDimension =
    dimension { width = px 100, height = px 100 }


constNumberOfBoxes : Int
constNumberOfBoxes =
    100


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type Model
    = NotReady NotReadyData
    | Ready Data


type alias NotReadyData =
    { windowDimension : Maybe Dimension, boxes : Maybe (List Box), time : Maybe Posix, apple : String }


type alias Data =
    { windowDimension : Dimension, boxes : List Box, time : Posix, showShadow : Bool, apple : String }


type alias Box =
    { coordinate : Coordinate, spinSpeed : Float }


randomBoxGenerator : Dimension -> Random.Generator Box
randomBoxGenerator windowDimension =
    let
        fromX =
            constBoxDimension |> Dimension.width |> Px.toInt |> (\n -> n // 2)

        toX =
            windowDimension |> Dimension.width |> Px.toInt |> (\n -> n + fromX)

        fromY =
            constBoxDimension |> Dimension.height |> Px.toInt |> (\n -> n // 2)

        toY =
            windowDimension |> Dimension.height |> Px.toInt |> (\n -> n + fromY)
    in
    Random.map3 (\x y spinSpeed -> { coordinate = coordinate { x = x, y = y }, spinSpeed = spinSpeed })
        (Px.randomGenerator -fromX toX)
        (Px.randomGenerator -fromY toY)
        (Random.float 0.75 1)


init : { apple : String } -> ( Model, Cmd Msg )
init { apple } =
    ( NotReady { windowDimension = Nothing, boxes = Nothing, time = Nothing, apple = apple }
    , Cmd.batch
        [ Browser.Dom.getViewport |> Task.perform GetViewportComplete
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrame AnimationFrameTick
        , Browser.Events.onResize UserResizeWindow
        ]



-- MSG


type Msg
    = AnimationFrameTick Posix
    | RandomGeneratorCompleteBoxes (List Box)
    | UserCheckShowShadowCheckBox Bool
    | UserResizeWindow Int Int
    | GetViewportComplete Browser.Dom.Viewport


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetViewportComplete viewport ->
            let
                windowDimension =
                    dimension
                        { width = px (round viewport.viewport.width)
                        , height = px (round viewport.viewport.height)
                        }
            in
            ( model
                |> setWindowDimension windowDimension
            , generateBoxes windowDimension
            )

        AnimationFrameTick time ->
            ( model |> setTime time, Cmd.none )

        UserCheckShowShadowCheckBox checked ->
            ( model |> setShowShadow checked, Cmd.none )

        RandomGeneratorCompleteBoxes boxes ->
            ( model |> setBoxes boxes, Cmd.none )

        UserResizeWindow width height ->
            let
                windowDimension =
                    dimension { width = px width, height = px height }
            in
            ( model |> setWindowDimension windowDimension
            , generateBoxes windowDimension
            )


generateBoxes : Dimension -> Cmd Msg
generateBoxes dimension =
    dimension
        |> randomBoxGenerator
        |> Random.list constNumberOfBoxes
        |> Random.generate RandomGeneratorCompleteBoxes


toReady : NotReadyData -> Model
toReady data =
    case ( data.windowDimension, data.boxes, data.time ) of
        ( Just windowDimension, Just boxes, Just time ) ->
            Ready
                { windowDimension = windowDimension
                , boxes = boxes
                , time = time
                , showShadow = False
                , apple = data.apple
                }

        _ ->
            NotReady data


setTime : Posix -> Model -> Model
setTime time model =
    case model of
        NotReady data ->
            { data | time = Just time }
                |> toReady

        Ready data ->
            Ready { data | time = time }


setShowShadow : Bool -> Model -> Model
setShowShadow showShadow model =
    case model of
        NotReady data ->
            data
                |> toReady

        Ready data ->
            Ready { data | showShadow = showShadow }


setBoxes : List Box -> Model -> Model
setBoxes boxes model =
    case model of
        NotReady data ->
            { data | boxes = Just boxes }
                |> toReady

        Ready data ->
            Ready { data | boxes = boxes }


setWindowDimension : Dimension -> Model -> Model
setWindowDimension dim model =
    case model of
        NotReady data ->
            { data | windowDimension = Just dim }
                |> toReady

        Ready data ->
            Ready { data | windowDimension = dim }



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        NotReady _ ->
            div [] [ text "Loading..." ]

        Ready data ->
            div []
                [ div [] (List.map (animatedBox data) data.boxes)
                , controlPanel data { onShowShadowCheck = UserCheckShowShadowCheckBox }
                ]


animatedBox : Data -> Box -> Html Msg
animatedBox data box =
    viewBox data
        box
        [ H.img
            [ HA.src data.apple
            , HA.style "width" "100%"
            , HA.style "height" "100%"
            ]
            []
        ]


timeToRotation : Float -> Posix -> Int
timeToRotation spinSpeed time =
    let
        millisecond =
            Time.posixToMillis time
    in
    if millisecond == 0 then
        0

    else
        modBy 3600 millisecond // round (10 * spinSpeed)


viewBox : Data -> Box -> List (Html Msg) -> Html Msg
viewBox data box children =
    let
        x =
            Coordinate.x box.coordinate

        y =
            Coordinate.y box.coordinate

        rotation =
            "rotate(" ++ (data.time |> timeToRotation box.spinSpeed |> String.fromInt) ++ "deg)"
    in
    div
        [ shadow data.showShadow
        , HA.style "width" (constBoxDimension |> Dimension.width |> Px.toString)
        , HA.style "height" (constBoxDimension |> Dimension.width |> Px.toString)
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


shadow : Bool -> H.Attribute Msg
shadow show =
    if show then
        HA.style "box-shadow" "0 0 10px rgba(0,0,0,0.5)"

    else
        HA.style "box-shadow" "none"
