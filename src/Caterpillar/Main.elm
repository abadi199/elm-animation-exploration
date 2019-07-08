module Caterpillar.Main exposing (main)

import AnimationType exposing (AnimationType)
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
import Millisecond exposing (Millisecond, millisecond)
import Px exposing (Px, px)
import Random
import Second exposing (second)
import Shared.ControlPanel as ControlPanel exposing (controlPanel)
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
    }


type alias Data =
    { flags : Flags
    , windowDimension : Dimension
    , boxes : List Box
    , showShadow : Bool
    , fps : Fps
    , animationType : AnimationType
    , caterpillarState : Caterpillar.State
    , cloud1State : Object.State
    , cloud2State : Object.State
    , hillFarState : Object.State
    , hillNearState : Object.State
    , treeState : Object.State
    , grassState : Object.State
    , bushState : Object.State
    , fenceState : Object.State
    }


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
        }
    , Cmd.batch
        [ Browser.Dom.getViewport |> Task.perform GetViewportComplete
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta (round >> millisecond >> AnimationFrameDeltaTick)
        , Browser.Events.onResize UserResizeWindow
        ]



-- MESSAGE


type Msg
    = AnimationFrameDeltaTick Millisecond
    | RandomGeneratorCompleteBoxes (List Box)
    | UserCheckShowShadowCheckBox Bool
    | UserChangeAnimationType AnimationType
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

        AnimationFrameDeltaTick animationFrameDelta ->
            ( model |> setAnimationState animationFrameDelta, Cmd.none )

        UserChangeAnimationType animationType ->
            ( model |> setAnimationType animationType, Cmd.none )

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
    case ( data.windowDimension, data.boxes ) of
        ( Just windowDimension, Just boxes ) ->
            Ready
                { flags = data.flags
                , windowDimension = windowDimension
                , boxes = boxes
                , showShadow = True
                , fps = Fps.initial
                , animationType = AnimationType.Elm
                , caterpillarState = Caterpillar.initialState
                , cloud1State = Object.initialState
                , cloud2State = Object.initialState
                , hillFarState = Object.initialState
                , hillNearState = Object.initialState
                , treeState = Object.initialState
                , grassState = Object.initialState
                , bushState = Object.initialState
                , fenceState = Object.initialState
                }

        _ ->
            NotReady data


setAnimationState : Millisecond -> Model -> Model
setAnimationState animationFrameDelta model =
    case model of
        NotReady data ->
            model

        Ready data ->
            case data.animationType of
                AnimationType.WebAnimation ->
                    Ready { data | fps = Fps.update animationFrameDelta data.fps }

                AnimationType.Elm ->
                    Ready
                        { data
                            | fps = Fps.update animationFrameDelta data.fps
                            , caterpillarState = Caterpillar.tick animationFrameDelta data.caterpillarState
                            , cloud1State = Object.tick animationFrameDelta data.cloud1State
                            , cloud2State = Object.tick animationFrameDelta data.cloud2State
                            , hillFarState = Object.tick animationFrameDelta data.hillFarState
                            , hillNearState = Object.tick animationFrameDelta data.hillNearState
                            , treeState = Object.tick animationFrameDelta data.treeState
                            , grassState = Object.tick animationFrameDelta data.grassState
                            , bushState = Object.tick animationFrameDelta data.bushState
                            , fenceState = Object.tick animationFrameDelta data.fenceState
                        }


setAnimationType : AnimationType -> Model -> Model
setAnimationType animationType model =
    case model of
        NotReady data ->
            data
                |> toReady

        Ready data ->
            Ready { data | animationType = animationType }


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

                objectView state =
                    case data.animationType of
                        AnimationType.Elm ->
                            Object.view state

                        AnimationType.WebAnimation ->
                            FastObject.view

                cloud1 =
                    objectView data.cloud1State
                        { imageUrl = data.flags.cloud1
                        , showShadow = data.showShadow
                        , windowDimension = windowDimension
                        , loopDuration = millisecond 60000
                        , dimension = windowDimension |> Dimension.setHeight (px 200)
                        , coordinate =
                            windowDimension
                                |> Dimension.toCoordinate
                                |> Coordinate.setX (px 0)
                                |> Coordinate.multiplyY 0.15
                        }

                cloud2 =
                    objectView data.cloud2State
                        { imageUrl = data.flags.cloud2
                        , showShadow = data.showShadow
                        , windowDimension = windowDimension
                        , loopDuration = millisecond 60000
                        , dimension = windowDimension |> Dimension.setHeight (px 200)
                        , coordinate =
                            windowDimension
                                |> Dimension.toCoordinate
                                |> Coordinate.setX (px 0)
                                |> Coordinate.multiplyY 0.05
                        }

                hillFar =
                    objectView data.hillFarState
                        { imageUrl = data.flags.hillFar
                        , showShadow = data.showShadow
                        , windowDimension = windowDimension
                        , loopDuration = millisecond 40000
                        , dimension = windowDimension |> Dimension.multiplyHeight 0.75
                        , coordinate =
                            windowDimension
                                |> Dimension.toCoordinate
                                |> Coordinate.setX (px 0)
                                |> Coordinate.multiplyY 0.3
                        }

                hillNear =
                    objectView data.hillNearState
                        { imageUrl = data.flags.hillNear
                        , showShadow = data.showShadow
                        , windowDimension = windowDimension
                        , loopDuration = millisecond 30000
                        , dimension = windowDimension |> Dimension.multiplyHeight 0.75
                        , coordinate =
                            windowDimension
                                |> Dimension.toCoordinate
                                |> Coordinate.setX (px 0)
                                |> Coordinate.multiplyY 0.3
                        }

                tree =
                    objectView data.treeState
                        { imageUrl = data.flags.tree
                        , showShadow = data.showShadow
                        , windowDimension = windowDimension
                        , loopDuration = millisecond 15000
                        , dimension = windowDimension |> Dimension.multiplyHeight 0.75
                        , coordinate =
                            windowDimension
                                |> Dimension.toCoordinate
                                |> Coordinate.setX (px 0)
                                |> Coordinate.multiplyY -0.05
                        }

                grass =
                    objectView data.grassState
                        { imageUrl = data.flags.grass
                        , showShadow = False
                        , windowDimension = windowDimension
                        , loopDuration = millisecond 5000
                        , dimension = windowDimension |> Dimension.multiplyHeight 0.5
                        , coordinate =
                            windowDimension
                                |> Dimension.toCoordinate
                                |> Coordinate.setX (px 0)
                                |> Coordinate.multiplyY 0.5
                        }

                bush =
                    objectView data.bushState
                        { imageUrl = data.flags.bush
                        , showShadow = data.showShadow
                        , windowDimension = windowDimension
                        , loopDuration = millisecond 9000
                        , dimension = windowDimension |> Dimension.setHeight (px 170)
                        , coordinate =
                            windowDimension
                                |> Dimension.toCoordinate
                                |> Coordinate.setX (px 0)
                                |> Coordinate.multiplyY 0.46
                        }

                fence =
                    objectView data.fenceState
                        { imageUrl = data.flags.fence
                        , showShadow = data.showShadow
                        , windowDimension = windowDimension
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
                    , time = Time.millisToPosix 0
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
                    , state = data.caterpillarState
                    }
                , controlPanel
                    |> ControlPanel.withShowShadowCheck UserCheckShowShadowCheckBox
                    |> ControlPanel.withAnimationType UserChangeAnimationType
                    |> ControlPanel.view data
                , Fps.view data
                ]
