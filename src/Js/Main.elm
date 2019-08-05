module Js.Main exposing (main)

import Browser
import Color exposing (Color)
import Coordinate exposing (Coordinate, coordinate)
import Count
import Degree exposing (deg)
import Html as H exposing (Html, div)
import Html.Attributes as HA
import Html.Events as HE
import Js.Animation as Animation
import Js.Animation.Options as Options
import Json.Decode as JD
import Millisecond exposing (millisecond)
import Percentage
import Px exposing (Px, px)
import Random
import Second exposing (second)
import Shared.ControlPanel as ControlPanel
import Svg as S exposing (..)
import Svg.Attributes as SA exposing (..)



-- MAIN


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- MODEL


type Model
    = NotReady
    | Ready Data


type alias Data =
    { boxes : List Box, showShadow : Bool }


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
        (Px.randomGenerator (Px.px 10) (Px.px 2000))
        (Px.randomGenerator (Px.px 10) (Px.px 1000))
        randomColorGenerator


init : () -> ( Model, Cmd Msg )
init flags =
    ( NotReady
    , Random.generate RandomGeneratorCompleteBoxes (Random.list 1000 randomBoxGenerator)
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MSG


type Msg
    = AnimationFinish
    | RandomGeneratorCompleteBoxes (List Box)
    | UserCheckShowShadowCheckBox Bool



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

        UserCheckShowShadowCheckBox _ ->
            ( NotReady, Cmd.none )

        RandomGeneratorCompleteBoxes boxes ->
            ( Ready { boxes = boxes, showShadow = False }, Cmd.none )


updateReady : Msg -> Data -> ( Model, Cmd Msg )
updateReady msg data =
    case msg of
        AnimationFinish ->
            let
                _ =
                    Debug.log "AnimationFinish" ""
            in
            ( Ready data, Cmd.none )

        UserCheckShowShadowCheckBox checked ->
            ( Ready { data | showShadow = checked }, Cmd.none )

        RandomGeneratorCompleteBoxes boxes ->
            ( Ready { data | boxes = boxes }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        NotReady ->
            div [] [ text "Loading..." ]

        Ready data ->
            div []
                [ div [] (List.map (animatedBox data) data.boxes)
                ]


animatedBox : Data -> Box -> Html Msg
animatedBox data box =
    Animation.node
        [ Animation.rotate (deg 0)
        , Animation.rotate (deg 360)
        ]
        (Options.default { duration = millisecond 2000 }
            |> Options.withIterations Count.infinite
        )
        [ HE.on "finish" (JD.succeed AnimationFinish) ]
        (viewBox data box [ text "JS" ])


viewBox : Data -> Box -> List (Html Msg) -> Html Msg
viewBox data box children =
    let
        x =
            Coordinate.x box.coordinate

        y =
            Coordinate.y box.coordinate
    in
    div
        [ shadow True
        , HA.style "background" (Color.toCssString box.color)
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


shadow : Bool -> H.Attribute Msg
shadow show =
    if show then
        HA.style "box-shadow" "0 0 10px rgba(0,0,0,1)"

    else
        HA.style "box-shadow" "none"
