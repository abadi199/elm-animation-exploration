module Caterpillar.Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Caterpillar.Caterpillar as Caterpillar
import Caterpillar.FastObject as FastObject
import Caterpillar.Object as Object
import Caterpillar.Sky as Sky
import Caterpillar.Sun as Sun
import Color exposing (Color)
import Coordinate exposing (Coordinate, coordinate)
import Count
import Degree exposing (deg)
import Dimension exposing (Dimension, dimension)
import Fps exposing (Fps)
import Html.Styled as H exposing (Html, div)
import Html.Styled.Attributes as HA
import Js.Animation as Animation
import Js.Animation.Options as Options
import Millisecond exposing (millisecond)
import Px exposing (Px, px)
import Random
import Second exposing (second)
import Shared.ControlPanel exposing (controlPanel)
import Task
import Time exposing (Posix)



-- CONSTANTS


constBoxDimension : Dimension
constBoxDimension =
    dimension { width = px 100, height = px 100 }


constNumberOfBoxes : Int
constNumberOfBoxes =
    0


main : Platform.Program Flags Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view >> H.toUnstyled
        }


type Model
    = NotReady NotReadyData
    | Ready Data


type alias NotReadyData =
    { flags : Flags
    , windowDimension : Maybe Dimension
    , boxes : Maybe (List Box)
    , time : Maybe Posix
    }


type alias Data =
    { flags : Flags
    , windowDimension : Dimension
    , boxes : List Box
    , time : Posix
    , showShadow : Bool
    , fps : Fps
    , animationType : AnimationType
    }


type AnimationType
    = Elm
    | WebAnimation


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
        (Random.float 5 10)


type alias Flags =
    { apple : String
    , caterpillar : String
    , sky : String
    , grass : String
    , fence : String
    , hillFar : String
    , hillNear : String
    , bush : String
    , sun : String
    , cloud1 : String
    , cloud2 : String
    , tree : String
    , apples : String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( NotReady
        { flags = flags
        , windowDimension = Nothing
        , boxes = Nothing
        , time = Nothing
        }
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



-- MESSAGE


type Msg
    = AnimationFrameTick Posix
    | RandomGeneratorCompleteBoxes (List Box)
    | UserCheckShowShadowCheckBox Bool
    | UserResizeWindow Int Int
    | GetViewportComplete Browser.Dom.Viewport



-- UPDATE


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
                { flags = data.flags
                , windowDimension = windowDimension
                , boxes = boxes
                , time = time
                , showShadow = True
                , fps = Fps.initial time
                , animationType = WebAnimation
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
            Ready { data | time = time, fps = Fps.update time data.fps }


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
            div [] [ H.text "Loading..." ]

        Ready data ->
            let
                windowDimension =
                    data.windowDimension

                cloud1 =
                    Object.view
                        { imageUrl = data.flags.cloud1
                        , showShadow = data.showShadow
                        , windowDimension = windowDimension
                        , time = data.time
                        , loopDuration = millisecond 60000
                        , dimension = windowDimension |> Dimension.setHeight (px 200)
                        , coordinate =
                            windowDimension
                                |> Dimension.toCoordinate
                                |> Coordinate.setX (px 0)
                                |> Coordinate.multiplyY 0.15
                        }

                cloud2 =
                    Object.view
                        { imageUrl = data.flags.cloud2
                        , showShadow = data.showShadow
                        , windowDimension = windowDimension
                        , time = data.time
                        , loopDuration = millisecond 60000
                        , dimension = windowDimension |> Dimension.setHeight (px 200)
                        , coordinate =
                            windowDimension
                                |> Dimension.toCoordinate
                                |> Coordinate.setX (px 0)
                                |> Coordinate.multiplyY 0.05
                        }

                hillFar =
                    Object.view
                        { imageUrl = data.flags.hillFar
                        , showShadow = data.showShadow
                        , windowDimension = windowDimension
                        , time = data.time
                        , loopDuration = millisecond 40000
                        , dimension = windowDimension |> Dimension.multiplyHeight 0.75
                        , coordinate =
                            windowDimension
                                |> Dimension.toCoordinate
                                |> Coordinate.setX (px 0)
                                |> Coordinate.multiplyY 0.3
                        }

                hillNear =
                    Object.view
                        { imageUrl = data.flags.hillNear
                        , showShadow = data.showShadow
                        , windowDimension = windowDimension
                        , time = data.time
                        , loopDuration = millisecond 30000
                        , dimension = windowDimension |> Dimension.multiplyHeight 0.75
                        , coordinate =
                            windowDimension
                                |> Dimension.toCoordinate
                                |> Coordinate.setX (px 0)
                                |> Coordinate.multiplyY 0.3
                        }

                tree =
                    Object.view
                        { imageUrl = data.flags.tree
                        , showShadow = data.showShadow
                        , windowDimension = windowDimension
                        , time = data.time
                        , loopDuration = millisecond 15000
                        , dimension = windowDimension |> Dimension.multiplyHeight 0.75
                        , coordinate =
                            windowDimension
                                |> Dimension.toCoordinate
                                |> Coordinate.setX (px 0)
                                |> Coordinate.multiplyY -0.05
                        }

                grass =
                    Object.view
                        { imageUrl = data.flags.grass
                        , showShadow = False
                        , windowDimension = windowDimension
                        , time = data.time
                        , loopDuration = millisecond 5000
                        , dimension = windowDimension |> Dimension.multiplyHeight 0.5
                        , coordinate =
                            windowDimension
                                |> Dimension.toCoordinate
                                |> Coordinate.setX (px 0)
                                |> Coordinate.multiplyY 0.5
                        }

                bush =
                    Object.view
                        { imageUrl = data.flags.bush
                        , showShadow = data.showShadow
                        , windowDimension = windowDimension
                        , time = data.time
                        , loopDuration = millisecond 9000
                        , dimension = windowDimension |> Dimension.setHeight (px 170)
                        , coordinate =
                            windowDimension
                                |> Dimension.toCoordinate
                                |> Coordinate.setX (px 0)
                                |> Coordinate.multiplyY 0.46
                        }

                fence =
                    Object.view
                        { imageUrl = data.flags.fence
                        , showShadow = data.showShadow
                        , windowDimension = windowDimension
                        , time = data.time
                        , loopDuration = millisecond 10000
                        , dimension = windowDimension |> Dimension.setHeight (px 150)
                        , coordinate =
                            windowDimension
                                |> Dimension.toCoordinate
                                |> Coordinate.setX (px 0)
                                |> Coordinate.multiplyY 0.42
                        }
            in
            div []
                [ Sky.view
                    { sky = data.flags.sky
                    , windowDimension = windowDimension
                    }
                , Sun.view
                    { imageUrl = data.flags.sun
                    , windowDimension = windowDimension
                    , time = data.time
                    , showShadow = data.showShadow
                    }
                , cloud1
                , cloud2
                , hillFar
                , hillNear
                , tree
                , grass
                , fence
                , bush
                , Caterpillar.view
                    { caterpillar = data.flags.caterpillar
                    , windowDimension = windowDimension
                    , showShadow = data.showShadow
                    }
                , H.fromUnstyled <| controlPanel data { onShowShadowCheck = UserCheckShowShadowCheckBox }
                , Fps.view data
                ]
