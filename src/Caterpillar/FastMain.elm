port module Caterpillar.FastMain exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Caterpillar.FastCaterpillar as Caterpillar
import Caterpillar.FastGrasses as Grasses exposing (Grass)
import Caterpillar.FastObject as Object
import Caterpillar.FastSun as Sun
import Caterpillar.Sky as Sky
import Coordinate
import Dimension exposing (Dimension, dimension)
import Html.Styled as H exposing (Html, div)
import Millisecond exposing (Millisecond, millisecond)
import Px exposing (Px, px)
import Random
import RotationSpeed
import Speed exposing (pxPerS)
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
    { flags : Flags
    , windowDimension : Dimension
    , grasses : List Grass
    , caterpillarState : Caterpillar.State
    , isAnimationPaused : Bool
    , isAppPaused : Bool
    }


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
            if data.isAppPaused then
                Sub.batch
                    [ Browser.Events.onResize UserResizeWindow
                    , pause PortPauseApp
                    ]

            else
                Sub.batch
                    [ Browser.Events.onResize UserResizeWindow
                    , pause PortPauseApp
                    ]


port pause : (Bool -> msg) -> Sub msg



-- MESSAGE


type Msg
    = RandomGeneratorCompleteGeneratingGrasses (List Grass)
    | UserResizeWindow Int Int
    | GetViewportComplete Browser.Dom.Viewport
    | CaterpillarFinishExpanding
    | CaterpillarFinishContracting
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
                |> setAnimationPaused False
            , Cmd.none
            )

        CaterpillarFinishContracting ->
            ( model
                |> setCaterpillarState Caterpillar.expanding
                |> setAnimationPaused True
            , Cmd.none
            )

        PortPauseApp isAppPaused ->
            ( model |> pauseApp isAppPaused, Cmd.none )


pauseApp : Bool -> Model -> Model
pauseApp isAppPaused model =
    case model of
        NotReady _ ->
            model

        Ready data ->
            Ready { data | isAppPaused = isAppPaused }


toReady : NotReadyData -> Model
toReady data =
    case ( data.windowDimension, data.grasses ) of
        ( Just windowDimension, Just grasses ) ->
            Ready
                { flags = data.flags
                , grasses = grasses
                , windowDimension = windowDimension
                , caterpillarState = Caterpillar.expanding
                , isAnimationPaused = True
                , isAppPaused = False
                }

        _ ->
            NotReady data


setAnimationPaused : Bool -> Model -> Model
setAnimationPaused isAnimationPaused model =
    case model of
        NotReady _ ->
            model

        Ready data ->
            Ready { data | isAnimationPaused = isAnimationPaused }


setCaterpillarState : Caterpillar.State -> Model -> Model
setCaterpillarState caterpillarState model =
    case model of
        NotReady _ ->
            model

        Ready data ->
            Ready { data | caterpillarState = caterpillarState }


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
                    True

                windowDimension =
                    data.windowDimension

                viewport =
                    dimension { width = px 1920, height = px 1080 }

                sun =
                    Sun.view
                        { sunUrl = data.flags.sun
                        , sunRaysUrl = data.flags.sunRays
                        , windowDimension = windowDimension
                        , time = Time.millisToPosix 0
                        , showShadow = showShadow
                        , rotationSpeed = RotationSpeed.degPerS 10
                        }

                cloud1 =
                    Object.view
                        { imageUrl = data.flags.cloud1
                        , name = "cloud1"
                        , isPaused = False
                        , showShadow = showShadow
                        , windowDimension = windowDimension
                        , speed = pxPerS 20
                        , dimension = windowDimension |> Dimension.setHeight (px 200)
                        , coordinate =
                            viewport
                                |> Dimension.toCoordinate
                                |> Coordinate.setX (px 0)
                                |> Coordinate.multiplyY 0.8
                        }

                cloud2 =
                    Object.view
                        { imageUrl = data.flags.cloud2
                        , name = "cloud2"
                        , isPaused = False
                        , showShadow = showShadow
                        , windowDimension = windowDimension
                        , speed = pxPerS 30
                        , dimension = windowDimension |> Dimension.setHeight (px 200)
                        , coordinate =
                            viewport
                                |> Dimension.toCoordinate
                                |> Coordinate.setX (px 0)
                                |> Coordinate.multiplyY 0.65
                        }

                hillFar =
                    Object.view
                        { imageUrl = data.flags.hillFar
                        , name = "hillFar"
                        , isPaused = data.isAnimationPaused
                        , showShadow = showShadow
                        , windowDimension = windowDimension
                        , speed = pxPerS -40
                        , dimension = windowDimension |> Dimension.setHeight (px 350)
                        , coordinate =
                            viewport
                                |> Dimension.toCoordinate
                                |> Coordinate.setX (px 0)
                                |> Coordinate.multiplyY 0.4
                        }

                hillNear =
                    Object.view
                        { imageUrl = data.flags.hillNear
                        , name = "hillNear"
                        , isPaused = data.isAnimationPaused
                        , showShadow = showShadow
                        , windowDimension = windowDimension
                        , speed = pxPerS -40
                        , dimension = windowDimension |> Dimension.setHeight (px 350)
                        , coordinate =
                            viewport
                                |> Dimension.toCoordinate
                                |> Coordinate.setX (px 0)
                                |> Coordinate.multiplyY 0.4
                        }

                tree =
                    Object.view
                        { imageUrl = data.flags.tree
                        , name = "tree"
                        , isPaused = data.isAnimationPaused
                        , showShadow = showShadow
                        , windowDimension = windowDimension
                        , speed = pxPerS -100
                        , dimension = windowDimension |> Dimension.multiplyHeight 0.75
                        , coordinate =
                            viewport
                                |> Dimension.toCoordinate
                                |> Coordinate.setX (px 0)
                                |> Coordinate.multiplyY 0.35
                        }

                grass =
                    Object.view
                        { imageUrl = data.flags.grass
                        , name = "grass"
                        , isPaused = data.isAnimationPaused
                        , showShadow = False
                        , windowDimension = windowDimension
                        , speed = pxPerS -400
                        , dimension = windowDimension |> Dimension.multiplyHeight 0.5
                        , coordinate =
                            viewport
                                |> Dimension.toCoordinate
                                |> Coordinate.setX (px 0)
                                |> Coordinate.setY (px 0)
                        }

                grasses =
                    Grasses.view
                        { grasses = data.grasses
                        , grassAllUrl = data.flags.grassAll
                        , showShadow = showShadow
                        , windowDimension = windowDimension
                        , imageWidth = imageWidth
                        , speed = pxPerS -450
                        , isPaused = data.isAnimationPaused
                        }

                bush =
                    Object.view
                        { imageUrl = data.flags.bush
                        , name = "bush"
                        , isPaused = data.isAnimationPaused
                        , showShadow = showShadow
                        , windowDimension = windowDimension
                        , speed = pxPerS -300
                        , dimension = windowDimension |> Dimension.setHeight (px 200)
                        , coordinate =
                            viewport
                                |> Dimension.toCoordinate
                                |> Coordinate.setX (px 0)
                                |> Coordinate.multiplyY 0.35
                        }

                fence =
                    Object.view
                        { imageUrl = data.flags.fence
                        , name = "fence"
                        , isPaused = data.isAnimationPaused
                        , showShadow = showShadow
                        , windowDimension = windowDimension
                        , speed = pxPerS -200
                        , dimension = windowDimension |> Dimension.setHeight (px 150)
                        , coordinate =
                            viewport
                                |> Dimension.toCoordinate
                                |> Coordinate.setX (px 0)
                                |> Coordinate.multiplyY 0.45
                        }

                sky =
                    Sky.view
                        { sky = data.flags.sky
                        , windowDimension = windowDimension
                        }

                caterpillar =
                    Caterpillar.view
                        { imageUrl = data.flags.caterpillar
                        , windowDimension = windowDimension
                        , showShadow = showShadow
                        , loopDuration = caterpillarLoopDuration
                        , onFinishExpanding = CaterpillarFinishExpanding
                        , onFinishContracting = CaterpillarFinishContracting
                        }
                        data.caterpillarState
            in
            div []
                [ sky
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