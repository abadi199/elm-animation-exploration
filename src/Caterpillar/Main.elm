module Caterpillar.Main exposing (main)

import AnimationType exposing (AnimationType)
import Browser
import Browser.Dom
import Browser.Events
import Caterpillar.Caterpillar as Caterpillar
import Caterpillar.FastObject as FastObject
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
import PxPerMs exposing (PxPerMs, pxPerMs)
import Random
import Second exposing (second)
import Shared.ControlPanel as ControlPanel exposing (controlPanel)
import Task
import Time exposing (Posix)



-- CONSTANTS


imageWidth : Px
imageWidth =
    Px.px 2048


caterpillarLoopDuration : Millisecond
caterpillarLoopDuration =
    millisecond 1000


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
    { flags : Flags
    , windowDimension : Dimension
    , showShadow : Bool
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
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta (round >> millisecond >> AnimationFrameDeltaTick)
        , Browser.Events.onResize UserResizeWindow
        ]



-- MESSAGE


type Msg
    = AnimationFrameDeltaTick Millisecond
    | RandomGeneratorCompleteGeneratingGrasses (List Grass)
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
            , Cmd.none
            )

        AnimationFrameDeltaTick animationFrameDelta ->
            ( model |> setAnimationState animationFrameDelta, Cmd.none )

        UserChangeAnimationType animationType ->
            ( model |> setAnimationType animationType, Cmd.none )

        UserCheckShowShadowCheckBox checked ->
            ( model |> setShowShadow checked, Cmd.none )

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


toReady : NotReadyData -> Model
toReady data =
    case ( data.windowDimension, data.grasses ) of
        ( Just windowDimension, Just grasses ) ->
            Ready
                { flags = data.flags
                , grasses = grasses
                , windowDimension = windowDimension
                , showShadow = True
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
                    , speed = pxPerMs -0.6
                    , rotationSpeed = 0
                    }

                grassesOptions =
                    { animationFrameDelta = animationFrameDelta
                    , speeds = data.grasses |> List.map (\grass -> ( grass.imageUrl, grass.speed )) |> Dict.fromList
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
                            , sunState = Sun.tick { options | rotationSpeed = 0.1 } data.sunState
                            , cloud1State = Object.continuousTick { options | speed = pxPerMs 0.02 } data.cloud1State
                            , cloud2State = Object.continuousTick { options | speed = pxPerMs 0.03 } data.cloud2State
                            , hillFarState = Object.tick { options | speed = pxPerMs -0.02 } data.hillFarState
                            , hillNearState = Object.tick { options | speed = pxPerMs -0.03 } data.hillNearState
                            , treeState = Object.tick { options | speed = pxPerMs -0.3 } data.treeState
                            , fenceState = Object.tick { options | speed = pxPerMs -0.4 } data.fenceState
                            , bushState = Object.tick { options | speed = pxPerMs -0.5 } data.bushState
                            , grassState = Object.tick { options | speed = pxPerMs -0.6 } data.grassState
                            , grassesState = Grasses.tick grassesOptions data.grassesState
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
                windowDimension =
                    data.windowDimension

                objectView state =
                    case data.animationType of
                        AnimationType.Elm ->
                            Object.view state

                        AnimationType.WebAnimation ->
                            FastObject.view

                sun =
                    Sun.view data.sunState
                        { sunUrl = data.flags.sun
                        , sunRaysUrl = data.flags.sunRays
                        , windowDimension = windowDimension
                        , time = Time.millisToPosix 0
                        , showShadow = data.showShadow
                        }

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

                grasses =
                    Grasses.view data.grassesState
                        { grasses = data.grasses
                        , showShadow = data.showShadow
                        , windowDimension = windowDimension
                        , loopDuration = millisecond 5000
                        , imageWidth = imageWidth
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
                , sun
                , cloud2
                , cloud1
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
                , grasses
                , controlPanel
                    |> ControlPanel.withShowShadowCheck UserCheckShowShadowCheckBox
                    |> ControlPanel.withAnimationType UserChangeAnimationType
                    |> ControlPanel.view data
                , Fps.view data
                ]
