port module Caterpillar.SlowMain exposing (main)

import AnimationType exposing (AnimationType)
import Browser
import Browser.Dom
import Browser.Events
import Caterpillar.Caterpillar as Caterpillar
import Caterpillar.Grasses as Grasses exposing (Grass)
import Caterpillar.Object as Object
import Caterpillar.Sky as Sky
import Caterpillar.Sun as Sun
import Color exposing (Color)
import Coordinate exposing (Coordinate, coordinate)
import Count
import Degree exposing (deg)
import Dict exposing (Dict)
import Dimension exposing (Dimension, dimension)
import Fps exposing (Fps)
import Html.Styled as H exposing (Html, div)
import Html.Styled.Attributes as HA
import Js.Animation as Animation
import Js.Animation.Options as Options
import Millisecond exposing (Millisecond, millisecond)
import Px exposing (Px, px)
import Random
import RotationSpeed exposing (RotationSpeed, degPerS)
import Second exposing (second)
import Shared.ControlPanel as ControlPanel
import Speed exposing (Speed, pxPerMs, pxPerS)
import Task
import Time exposing (Posix)



-- CONSTANTS


imageWidth : Px
imageWidth =
    Px.px 2048


caterpillarLoopDuration : Millisecond
caterpillarLoopDuration =
    millisecond 2000


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
    , grasses : Maybe (List Grass)
    }


type alias Data =
    { controlPanelState : ControlPanel.State
    , flags : Flags
    , windowDimension : Dimension
    , fps : Fps
    , animationType : AnimationType
    , caterpillarState : Caterpillar.State
    , sunState : Sun.State
    , cloud1State : Object.State
    , cloud2State : Object.State
    , hillFarState : Object.State
    , hillNearState : Object.State
    , treeState : Object.State
    , grassState : Object.State
    , bushState : Object.State
    , fenceState : Object.State
    , grasses : List Grass
    , grassesState : Grasses.State
    , isPaused : Bool
    }


type alias Box =
    { coordinate : Coordinate, spinSpeed : Float }


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
    , sunRays : String
    , cloud1 : String
    , cloud2 : String
    , tree : String
    , apples : String
    , grassAll : String
    , grasses : List String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( NotReady
        { flags = flags
        , windowDimension = Nothing
        , grasses = Nothing
        }
    , Cmd.batch
        [ Browser.Dom.getViewport |> Task.perform GetViewportComplete
        , generateGrasses flags.grasses
        ]
    )


generateGrasses : List String -> Cmd Msg
generateGrasses grasses =
    let
        numberOfGrasses =
            List.length grasses

        sequence =
            List.foldr (Random.map2 (::)) (Random.constant [])
    in
    grasses
        |> List.map Grasses.randomGenerator
        |> sequence
        |> Random.generate RandomGeneratorCompleteGeneratingGrasses


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        NotReady _ ->
            Sub.none

        Ready data ->
            if data.isPaused then
                pause PortPauseApp

            else
                Sub.batch
                    [ Browser.Events.onAnimationFrameDelta (round >> millisecond >> AnimationFrameDeltaTick)
                    , Browser.Events.onResize UserResizeWindow
                    , pause PortPauseApp
                    ]


port pause : (Bool -> msg) -> Sub msg



-- MESSAGE


type Msg
    = AnimationFrameDeltaTick Millisecond
    | RandomGeneratorCompleteGeneratingGrasses (List Grass)
    | UserUpdateControlPanel ControlPanel.State
    | UserResizeWindow Int Int
    | GetViewportComplete Browser.Dom.Viewport
    | PortPauseApp Bool



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
            , Cmd.none
            )

        AnimationFrameDeltaTick animationFrameDelta ->
            ( model |> setAnimationState animationFrameDelta, Cmd.none )

        UserUpdateControlPanel controlPanelState ->
            ( model |> setControlPanelState controlPanelState, Cmd.none )

        RandomGeneratorCompleteGeneratingGrasses grasses ->
            ( model |> setGrasses grasses, Cmd.none )

        UserResizeWindow width height ->
            let
                windowDimension =
                    dimension { width = px width, height = px height }
            in
            ( model |> setWindowDimension windowDimension
            , Cmd.none
            )

        PortPauseApp isPaused ->
            ( model |> pauseApp isPaused, Cmd.none )


pauseApp : Bool -> Model -> Model
pauseApp isPaused model =
    case model of
        NotReady _ ->
            model

        Ready data ->
            Ready { data | isPaused = isPaused }


toReady : NotReadyData -> Model
toReady data =
    case ( data.windowDimension, data.grasses ) of
        ( Just windowDimension, Just grasses ) ->
            Ready
                { controlPanelState =
                    ControlPanel.initialState
                        |> ControlPanel.withShowShadow True
                , flags = data.flags
                , grasses = grasses
                , windowDimension = windowDimension
                , fps = Fps.initial
                , animationType = AnimationType.Elm
                , caterpillarState = Caterpillar.initialState
                , sunState = Sun.initialState
                , cloud1State = Object.initialState
                , cloud2State = Object.initialState
                , hillFarState = Object.initialState
                , hillNearState = Object.initialState
                , treeState = Object.initialState
                , grassState = Object.initialState
                , bushState = Object.initialState
                , fenceState = Object.initialState
                , grassesState = Grasses.initialState grasses
                , isPaused = False
                }

        _ ->
            NotReady data


setAnimationState : Millisecond -> Model -> Model
setAnimationState animationFrameDelta model =
    case model of
        NotReady data ->
            model

        Ready data ->
            let
                options =
                    { animationFrameDelta = animationFrameDelta
                    , loopDuration = caterpillarLoopDuration
                    , windowDimension = data.windowDimension
                    , speed = pxPerMs 0
                    , rotationSpeed = degPerS 0
                    }

                grassesOptions =
                    { animationFrameDelta = animationFrameDelta
                    , speeds =
                        data.grasses
                            |> List.map (\grass -> ( grass.imageUrl, grass.speed ))
                            |> Dict.fromList
                    , loopDuration = caterpillarLoopDuration
                    , windowDimension = data.windowDimension
                    , speed = pxPerS -450
                    , rotationSpeed = degPerS 0
                    }
            in
            case data.animationType of
                AnimationType.WebAnimation ->
                    Ready { data | fps = Fps.update animationFrameDelta data.fps }

                AnimationType.Elm ->
                    Ready
                        { data
                            | fps = Fps.update animationFrameDelta data.fps
                            , caterpillarState = Caterpillar.tick options data.caterpillarState
                            , sunState = Sun.tick { options | rotationSpeed = degPerS 10 } data.sunState
                            , cloud1State = Object.continuousTick { options | speed = pxPerS 20 } data.cloud1State
                            , cloud2State = Object.continuousTick { options | speed = pxPerS 30 } data.cloud2State
                            , hillFarState = Object.tick { options | speed = pxPerS -30 } data.hillFarState
                            , hillNearState = Object.tick { options | speed = pxPerS -50 } data.hillNearState
                            , treeState = Object.tick { options | speed = pxPerS -100 } data.treeState
                            , fenceState = Object.tick { options | speed = pxPerS -200 } data.fenceState
                            , bushState = Object.tick { options | speed = pxPerS -300 } data.bushState
                            , grassState = Object.tick { options | speed = pxPerS -400 } data.grassState
                            , grassesState = Grasses.tick grassesOptions data.grassesState
                        }


setControlPanelState : ControlPanel.State -> Model -> Model
setControlPanelState controlPanelState model =
    case model of
        NotReady data ->
            data
                |> toReady

        Ready data ->
            Ready { data | controlPanelState = controlPanelState }


setGrasses : List Grass -> Model -> Model
setGrasses grasses model =
    case model of
        NotReady data ->
            { data | grasses = Just grasses }
                |> toReady

        Ready data ->
            Ready { data | grasses = grasses }


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
                showShadow =
                    data.controlPanelState
                        |> ControlPanel.showShadow
                        |> Maybe.withDefault True

                windowDimension =
                    data.windowDimension

                sun =
                    Sun.view data.sunState
                        { sunUrl = data.flags.sun
                        , sunRaysUrl = data.flags.sunRays
                        , windowDimension = windowDimension
                        , time = Time.millisToPosix 0
                        , showShadow = showShadow
                        }

                cloud1 =
                    Object.view data.cloud1State
                        { imageUrl = data.flags.cloud1
                        , showShadow = showShadow
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
                    Object.view data.cloud2State
                        { imageUrl = data.flags.cloud2
                        , showShadow = showShadow
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
                    Object.view data.hillFarState
                        { imageUrl = data.flags.hillFar
                        , showShadow = showShadow
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
                    Object.view data.hillNearState
                        { imageUrl = data.flags.hillNear
                        , showShadow = showShadow
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
                    Object.view data.treeState
                        { imageUrl = data.flags.tree
                        , showShadow = showShadow
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
                    Object.view data.grassState
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

                grasses =
                    Grasses.view data.grassesState
                        { grasses = data.grasses
                        , grassAllUrl = data.flags.grassAll
                        , showShadow = showShadow
                        , windowDimension = windowDimension
                        , loopDuration = millisecond 5000
                        , imageWidth = imageWidth
                        }

                bush =
                    Object.view data.bushState
                        { imageUrl = data.flags.bush
                        , showShadow = showShadow
                        , windowDimension = windowDimension
                        , loopDuration = millisecond 9000
                        , dimension = windowDimension |> Dimension.setHeight (px 200)
                        , coordinate =
                            windowDimension
                                |> Dimension.toCoordinate
                                |> Coordinate.setX (px 0)
                                |> Coordinate.multiplyY 0.46
                        }

                fence =
                    Object.view data.fenceState
                        { imageUrl = data.flags.fence
                        , showShadow = showShadow
                        , windowDimension = windowDimension
                        , loopDuration = millisecond 10000
                        , dimension = windowDimension |> Dimension.setHeight (px 150)
                        , coordinate =
                            windowDimension
                                |> Dimension.toCoordinate
                                |> Coordinate.setX (px 0)
                                |> Coordinate.multiplyY 0.42
                        }

                caterpillar =
                    Caterpillar.view
                        { caterpillar = data.flags.caterpillar
                        , windowDimension = windowDimension
                        , showShadow = showShadow
                        , state = data.caterpillarState
                        }
            in
            div []
                [ Sky.view
                    { sky = data.flags.sky
                    , windowDimension = windowDimension
                    }
                , sun
                , cloud2
                , cloud1
                , hillFar
                , hillNear
                , tree
                , grass
                , fence
                , bush
                , caterpillar
                , grasses
                , ControlPanel.view UserUpdateControlPanel data.controlPanelState
                , Fps.view data
                , Dimension.view windowDimension
                ]
