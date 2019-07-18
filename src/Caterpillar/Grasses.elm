module Caterpillar.Grasses exposing
    ( Grass
    , State
    , initialState
    , randomGenerator
    , tick
    , view
    )

import Caterpillar.Shadow as Shadow
import Css exposing (..)
import Css.Grid as Grid exposing (displayGrid)
import DegPerMs exposing (DegPerMs, degPerS)
import Degree exposing (Degree, deg)
import Dict exposing (Dict)
import Dimension exposing (Dimension)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as HA
import Millisecond exposing (Millisecond, millisecond)
import Px exposing (Px)
import Random
import Velocity exposing (Velocity)



--STATE


maxRotation : Degree
maxRotation =
    deg 30


type State
    = State StateData


type alias Grass =
    { imageUrl : String
    , speed : DegPerMs
    }


type alias StateData =
    { translationTimer : Millisecond
    , rotations : Dict String ( Degree, Direction )
    , positionX : Px
    }


type Direction
    = Left
    | Right


initialState : List Grass -> State
initialState grasses =
    State
        { translationTimer = millisecond 0
        , rotations = grasses |> List.map (\grass -> ( grass.imageUrl, ( deg 0, Right ) )) |> Dict.fromList
        , positionX = Px.px 0
        }



-- TICK


type alias TickOptions a =
    { a
        | animationFrameDelta : Millisecond
        , speeds : Dict String DegPerMs
        , windowDimension : Dimension
        , loopDuration : Millisecond
        , speed : Velocity
    }


tick : TickOptions a -> State -> State
tick { animationFrameDelta, windowDimension, speed, speeds, loopDuration } (State stateData) =
    let
        translationTimer =
            stateData.translationTimer
                |> Millisecond.add animationFrameDelta
                |> Millisecond.modBy loopDuration

        halfLoop =
            loopDuration |> Millisecond.multiply 0.6

        positionX =
            if translationTimer |> Millisecond.is (>) halfLoop then
                let
                    windowWidth =
                        windowDimension |> Dimension.width

                    newPositionX =
                        stateData.positionX
                            |> Px.add (speed |> Velocity.distance animationFrameDelta)
                in
                if newPositionX |> Px.is (<) (Px.map negate windowWidth) then
                    Px.px 0

                else
                    newPositionX

            else
                stateData.positionX
    in
    State
        { stateData
            | rotations = stateData.rotations |> Dict.map (updateRotation speeds animationFrameDelta)
            , translationTimer = translationTimer
            , positionX = positionX
        }


updateRotation : Dict String DegPerMs -> Millisecond -> String -> ( Degree, Direction ) -> ( Degree, Direction )
updateRotation speeds animationFrameDelta imageUrl ( rotation, direction ) =
    let
        speed =
            speeds |> Dict.get imageUrl |> Maybe.withDefault (DegPerMs.degPerS 0)

        moveToRight =
            rotation
                |> Degree.add (DegPerMs.toDegree animationFrameDelta speed)

        moveToLeft =
            rotation
                |> Degree.subtract (DegPerMs.toDegree animationFrameDelta speed)
    in
    case direction of
        Right ->
            if rotation |> Degree.is (>) maxRotation then
                ( moveToLeft
                , Left
                )

            else
                ( moveToRight
                , Right
                )

        Left ->
            if rotation |> Degree.is (<=) (Degree.deg 0) then
                ( moveToRight
                , Right
                )

            else
                ( moveToLeft
                , Left
                )


type alias Options =
    { grasses : List Grass
    , grassAllUrl : String
    , windowDimension : Dimension
    , loopDuration : Millisecond
    , showShadow : Bool
    , imageWidth : Px
    }



-- VIEW


view : State -> Options -> Html msg
view (State stateData) ({ grasses, grassAllUrl, windowDimension, imageWidth } as options) =
    let
        windowWidth =
            windowDimension |> Dimension.width

        ratio =
            Px.toFloat imageWidth / Px.toFloat windowWidth

        grassCount =
            List.length grasses

        columnWidth =
            windowWidth |> Px.divideBy grassCount

        grassAll =
            H.div
                [ HA.css
                    [ position absolute
                    , width (pct 100)
                    , height (pct 100)
                    , top (px 0)
                    , left (pct 0)
                    ]
                ]
                [ H.div
                    [ HA.css
                        [ position absolute
                        , width (pct 100)
                        , height (pct 100)
                        , backgroundImage (url grassAllUrl)
                        , Shadow.style options.showShadow
                        , backgroundRepeat2 repeat noRepeat
                        , backgroundPosition2 zero (pct -50)
                        ]
                    ]
                    []
                , H.div
                    [ HA.css
                        [ position absolute
                        , width (pct 100)
                        , height (pct 100)
                        , backgroundImage (url grassAllUrl)
                        , Shadow.style options.showShadow
                        , backgroundRepeat2 repeat noRepeat
                        , backgroundPosition2 (pct 25) (pct -50)
                        ]
                    ]
                    []
                , H.div
                    [ HA.css
                        [ position absolute
                        , width (pct 100)
                        , height (pct 100)
                        , backgroundImage (url grassAllUrl)
                        , Shadow.style options.showShadow
                        , backgroundRepeat2 repeat noRepeat
                        , backgroundPosition2 (pct 75) (pct -50)
                        ]
                    ]
                    []
                , H.div
                    [ HA.css
                        [ position absolute
                        , width (pct 100)
                        , height (pct 100)
                        , backgroundImage (url grassAllUrl)
                        , Shadow.style options.showShadow
                        , backgroundRepeat2 repeat noRepeat
                        , backgroundPosition2 (pct 50) (pct -50)
                        ]
                    ]
                    []
                ]

        gridColumns =
            grasses
                |> List.map (always (columnWidth |> Px.toString))
                |> String.join " "

        grassesView =
            grasses |> List.map (.imageUrl >> viewGrass ratio stateData options)
    in
    H.div
        [ HA.css
            [ width (pct 300)
            , height (pct 30)
            , position absolute
            , left (stateData.positionX |> Px.add (Px.map negate windowWidth) |> Px.toElmCss)
            , bottom (px -15)
            , displayGrid
            , property "grid-template-columns" <|
                gridColumns
                    ++ " "
                    ++ gridColumns
                    ++ " "
                    ++ gridColumns
            ]
        ]
        (grassAll :: grassesView ++ grassesView ++ grassesView)


viewGrass : Float -> StateData -> Options -> String -> Html msg
viewGrass ratio stateData options imageUrl =
    let
        rotation =
            stateData.rotations
                |> Dict.get imageUrl
                |> Maybe.map Tuple.first
                |> Maybe.withDefault (Degree.deg 0)
    in
    H.div
        [ HA.css
            [ backgroundImage (url imageUrl)
            , property "background-position" "center bottom"
            , height (pct 100)
            , width (px (200 * ratio))
            , backgroundSize contain
            , backgroundRepeat noRepeat
            , Shadow.style options.showShadow
            , property "transform-origin" "50% 100%"
            , property "justify-self" "center"
            , transform
                (rotation
                    |> Degree.toFloat
                    |> Css.deg
                    |> rotate
                )
            ]
        ]
        []



-- RANDOM


randomGenerator : String -> Random.Generator Grass
randomGenerator imageUrl =
    let
        from =
            degPerS 10

        to =
            degPerS 30
    in
    Random.map (\speed -> { imageUrl = imageUrl, speed = speed })
        (DegPerMs.randomGenerator from to)
