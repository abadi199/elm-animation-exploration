module Caterpillar.FastMain exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Caterpillar.FastCaterpillar as Caterpillar
import Caterpillar.FastObject as Object
import Caterpillar.Grasses as Grasses exposing (Grass)
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
import Second exposing (second)
import Shared.ControlPanel as ControlPanel
import Task
import Time exposing (Posix)
import Velocity exposing (Velocity, pxPerMs, pxPerS)



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
    { controlPanelState : ControlPanel.State
    , flags : Flags
    , windowDimension : Dimension
    , fps : Fps
    , sunState : Sun.State
    , grasses : List Grass
    , grassesState : Grasses.State
    , caterpillarState : Caterpillar.State
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
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta (round >> millisecond >> AnimationFrameDeltaTick)
        , Browser.Events.onResize UserResizeWindow
        ]



-- MESSAGE


type Msg
    = AnimationFrameDeltaTick Millisecond
    | RandomGeneratorCompleteGeneratingGrasses (List Grass)
    | UserUpdateControlPanel ControlPanel.State
    | UserResizeWindow Int Int
    | GetViewportComplete Browser.Dom.Viewport
    | CaterpillarFinishExpanding
    | CaterpillarFinishContracting



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

        CaterpillarFinishExpanding ->
            ( model
                |> setCaterpillarState Caterpillar.contracting
                |> setPaused False
            , Cmd.none
            )

        CaterpillarFinishContracting ->
            ( model
                |> setCaterpillarState Caterpillar.expanding
                |> setPaused True
            , Cmd.none
            )


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
                , sunState = Sun.initialState
                , grassesState = Grasses.initialState grasses
                , caterpillarState = Caterpillar.expanding
                , isPaused = True
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
                    , speeds =
                        data.grasses
                            |> List.map (\grass -> ( grass.imageUrl, grass.speed ))
                            |> Dict.fromList
                    , loopDuration = caterpillarLoopDuration
                    , windowDimension = data.windowDimension
                    , speed = pxPerMs -0.7
                    , rotationSpeed = 0
                    }
            in
            Ready
                { data
                    | fps = Fps.update animationFrameDelta data.fps
                    , sunState = Sun.tick { options | rotationSpeed = 0.1 } data.sunState
                    , grassesState = Grasses.tick grassesOptions data.grassesState
                }


setPaused : Bool -> Model -> Model
setPaused isPaused model =
    case model of
        NotReady _ ->
            model

        Ready data ->
            Ready { data | isPaused = isPaused }


setCaterpillarState : Caterpillar.State -> Model -> Model
setCaterpillarState caterpillarState model =
    case model of
        NotReady _ ->
            model

        Ready data ->
            Ready { data | caterpillarState = caterpillarState }


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
                    Object.view
                        { imageUrl = data.flags.cloud1
                        , isPaused = False
                        , showShadow = showShadow
                        , windowDimension = windowDimension
                        , speed = pxPerS 20
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
                        , isPaused = False
                        , showShadow = showShadow
                        , windowDimension = windowDimension
                        , speed = pxPerS 30
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
                        , isPaused = data.isPaused
                        , showShadow = showShadow
                        , windowDimension = windowDimension
                        , speed = pxPerS -30
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
                        , isPaused = data.isPaused
                        , showShadow = showShadow
                        , windowDimension = windowDimension
                        , speed = pxPerS -50
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
                        , isPaused = data.isPaused
                        , showShadow = showShadow
                        , windowDimension = windowDimension
                        , speed = pxPerS -100
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
                        , isPaused = data.isPaused
                        , showShadow = False
                        , windowDimension = windowDimension
                        , speed = pxPerS -400
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
                    Object.view
                        { imageUrl = data.flags.bush
                        , isPaused = data.isPaused
                        , showShadow = showShadow
                        , windowDimension = windowDimension
                        , speed = pxPerS -300
                        , dimension = windowDimension |> Dimension.setHeight (px 200)
                        , coordinate =
                            windowDimension
                                |> Dimension.toCoordinate
                                |> Coordinate.setX (px 0)
                                |> Coordinate.multiplyY 0.46
                        }

                fence =
                    Object.view
                        { imageUrl = data.flags.fence
                        , isPaused = data.isPaused
                        , showShadow = showShadow
                        , windowDimension = windowDimension
                        , speed = pxPerS -200
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

                -- , sun
                , cloud2
                , cloud1
                , hillFar
                , hillNear
                , tree
                , grass
                , fence
                , bush
                , Caterpillar.view
                    { imageUrl = data.flags.caterpillar
                    , windowDimension = windowDimension
                    , showShadow = showShadow
                    , loopDuration = millisecond 1000
                    , onFinishExpanding = CaterpillarFinishExpanding
                    , onFinishContracting = CaterpillarFinishContracting
                    }
                    data.caterpillarState

                -- , grasses
                , ControlPanel.view UserUpdateControlPanel data.controlPanelState
                , Fps.view data
                ]
