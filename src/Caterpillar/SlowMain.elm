port module Caterpillar.SlowMain exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Caterpillar.Caterpillar as Caterpillar
import Caterpillar.Grasses as Grasses exposing (Grass)
import Caterpillar.Object as Object
import Caterpillar.Sky as Sky
import Caterpillar.Sun as Sun
import Coordinate
import Css
import Dict
import Dimension exposing (Dimension, dimension)
import Html.Styled as H exposing (Html, div)
import Html.Styled.Attributes as HA
import Html.Styled.Events as HE
import Millisecond exposing (Millisecond, millisecond)
import Px exposing (Px, px)
import Random
import RotationSpeed exposing (degPerS)
import Speed exposing (pxPerMs, pxPerS)
import Task
import Time



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
    { showShadow : Bool
    , stage : Stage
    , flags : Flags
    , windowDimension : Dimension
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


type Stage
    = CaterpillarSoloStill
    | WithBackgroundStill
    | AllObjectsWithShadowMoving
    | WithBackgroundMoving


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
                Sub.batch
                    [ Browser.Events.onResize UserResizeWindow
                    , pause PortPauseApp
                    ]

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
    | UserResizeWindow Int Int
    | GetViewportComplete Browser.Dom.Viewport
    | PortPauseApp Bool
    | UserClickMouse



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

        UserClickMouse ->
            ( model |> updateStage, Cmd.none )


updateStage : Model -> Model
updateStage model =
    case model of
        NotReady _ ->
            model

        Ready data ->
            let newStage = nextStage data.stage
            in
            Ready
                { data
                    | stage = newStage
                    , showShadow = shouldShowShadow newStage
                }


shouldShowShadow : Stage -> Bool
shouldShowShadow stage =
    case stage of
        CaterpillarSoloStill ->
            False

        WithBackgroundStill ->
            False

        WithBackgroundMoving ->
            False

        AllObjectsWithShadowMoving ->
            True


nextStage : Stage -> Stage
nextStage stage =
    case stage of
        CaterpillarSoloStill ->
            WithBackgroundStill

        WithBackgroundStill ->
            WithBackgroundMoving

        WithBackgroundMoving ->
            AllObjectsWithShadowMoving

        AllObjectsWithShadowMoving ->
            AllObjectsWithShadowMoving


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
                { showShadow = False
                , stage = CaterpillarSoloStill
                , flags = data.flags
                , grasses = grasses
                , windowDimension = windowDimension
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
        NotReady _ ->
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

                move =
                    Ready
                        { data
                            | caterpillarState = Caterpillar.tick options data.caterpillarState
                            , sunState = Sun.tick { options | rotationSpeed = degPerS 10 } data.sunState
                            , cloud1State = Object.continuousTick { options | speed = pxPerS 20 } data.cloud1State
                            , cloud2State = Object.continuousTick { options | speed = pxPerS 30 } data.cloud2State
                            , hillFarState = Object.tick { options | speed = pxPerS -40 } data.hillFarState
                            , hillNearState = Object.tick { options | speed = pxPerS -40 } data.hillNearState
                            , treeState = Object.tick { options | speed = pxPerS -100 } data.treeState
                            , fenceState = Object.tick { options | speed = pxPerS -200 } data.fenceState
                            , bushState = Object.tick { options | speed = pxPerS -300 } data.bushState
                            , grassState = Object.tick { options | speed = pxPerS -400 } data.grassState
                            , grassesState = Grasses.tick grassesOptions data.grassesState
                        }

                still =
                    Ready data
            in
            case data.stage of
                CaterpillarSoloStill ->
                    still

                WithBackgroundStill ->
                    still

                WithBackgroundMoving ->
                    move

                AllObjectsWithShadowMoving ->
                    move


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
                    data.showShadow

                windowDimension =
                    data.windowDimension

                viewport =
                    dimension { width = px 1920, height = px 1080 }

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
                        , name = "cloud1"
                        , showShadow = showShadow
                        , windowDimension = windowDimension
                        , loopDuration = millisecond 60000
                        , dimension = windowDimension |> Dimension.setHeight (px 200)
                        , coordinate =
                            viewport
                                |> Dimension.toCoordinate
                                |> Coordinate.setX (px 0)
                                |> Coordinate.multiplyY 0.8
                        }

                cloud2 =
                    Object.view data.cloud2State
                        { imageUrl = data.flags.cloud2
                        , name = "cloud2"
                        , showShadow = showShadow
                        , windowDimension = windowDimension
                        , loopDuration = millisecond 60000
                        , dimension = windowDimension |> Dimension.setHeight (px 200)
                        , coordinate =
                            viewport
                                |> Dimension.toCoordinate
                                |> Coordinate.setX (px 0)
                                |> Coordinate.multiplyY 0.65
                        }

                hillFar =
                    Object.view data.hillFarState
                        { imageUrl = data.flags.hillFar
                        , name = "hillFar"
                        , showShadow = showShadow
                        , windowDimension = windowDimension
                        , loopDuration = millisecond 40000
                        , dimension = windowDimension |> Dimension.setHeight (px 350)
                        , coordinate =
                            viewport
                                |> Dimension.toCoordinate
                                |> Coordinate.setX (px 0)
                                |> Coordinate.multiplyY 0.4
                        }

                hillNear =
                    Object.view data.hillNearState
                        { imageUrl = data.flags.hillNear
                        , name = "hillNear"
                        , showShadow = showShadow
                        , windowDimension = windowDimension
                        , loopDuration = millisecond 40000
                        , dimension = windowDimension |> Dimension.setHeight (px 350)
                        , coordinate =
                            viewport
                                |> Dimension.toCoordinate
                                |> Coordinate.setX (px 0)
                                |> Coordinate.multiplyY 0.4
                        }

                tree =
                    Object.view data.treeState
                        { imageUrl = data.flags.tree
                        , name = "tree"
                        , showShadow = showShadow
                        , windowDimension = windowDimension
                        , loopDuration = millisecond 15000
                        , dimension = windowDimension |> Dimension.setHeight (px 750)
                        , coordinate =
                            viewport
                                |> Dimension.toCoordinate
                                |> Coordinate.setX (px 0)
                                |> Coordinate.multiplyY 0.35
                        }

                grass =
                    Object.view data.grassState
                        { imageUrl = data.flags.grass
                        , name = "grass"
                        , showShadow = False
                        , windowDimension = windowDimension
                        , loopDuration = millisecond 5000
                        , dimension = windowDimension |> Dimension.setHeight (px 540)
                        , coordinate =
                            viewport
                                |> Dimension.toCoordinate
                                |> Coordinate.setX (px 0)
                                |> Coordinate.setY (px 0)
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
                        , name = "bush"
                        , showShadow = showShadow
                        , windowDimension = windowDimension
                        , loopDuration = millisecond 9000
                        , dimension = windowDimension |> Dimension.setHeight (px 200)
                        , coordinate =
                            viewport
                                |> Dimension.toCoordinate
                                |> Coordinate.setX (px 0)
                                |> Coordinate.multiplyY 0.35
                        }

                fence =
                    Object.view data.fenceState
                        { imageUrl = data.flags.fence
                        , name = "fence"
                        , showShadow = showShadow
                        , windowDimension = windowDimension
                        , loopDuration = millisecond 10000
                        , dimension = windowDimension |> Dimension.setHeight (px 150)
                        , coordinate =
                            viewport
                                |> Dimension.toCoordinate
                                |> Coordinate.setX (px 0)
                                |> Coordinate.multiplyY 0.45
                        }

                caterpillar =
                    Caterpillar.view
                        { caterpillar = data.flags.caterpillar
                        , windowDimension = windowDimension
                        , showShadow = showShadow
                        , state = data.caterpillarState
                        }

                canvas =
                    div
                        [ HE.onClick UserClickMouse
                        , HA.css
                            [ Css.position Css.absolute
                            , Css.width (Css.vw 100)
                            , Css.height (Css.vh 100)
                            , Css.top (Css.px 0)
                            , Css.left (Css.px 0)
                            ]
                        ]

                viewAll =
                    canvas
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
                        ]

                viewWithBackground =
                    canvas
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
                        ]

                viewCaterpillar =
                    canvas
                        [ caterpillar ]
            in
            case data.stage of
                CaterpillarSoloStill ->
                    viewCaterpillar

                WithBackgroundStill ->
                    viewWithBackground

                WithBackgroundMoving ->
                    viewWithBackground

                AllObjectsWithShadowMoving ->
                    viewAll
