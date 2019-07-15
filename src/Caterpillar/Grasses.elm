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
    { timer : Millisecond
    , rotations : Dict String ( Degree, Direction )
    }


type Direction
    = Left
    | Right


initialState : List Grass -> State
initialState grasses =
    State
        { timer = millisecond 0
        , rotations = grasses |> List.map (\grass -> ( grass.imageUrl, ( deg 0, Right ) )) |> Dict.fromList
        }



-- TICK


type alias TickOptions a =
    { a
        | animationFrameDelta : Millisecond
        , speeds : Dict String DegPerMs
    }


tick : TickOptions a -> State -> State
tick { animationFrameDelta, speeds } (State stateData) =
    State
        { stateData
            | timer = stateData.timer |> Millisecond.add animationFrameDelta
            , rotations = stateData.rotations |> Dict.map (updateRotation speeds animationFrameDelta)
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
    , windowDimension : Dimension
    , loopDuration : Millisecond
    , showShadow : Bool
    , imageWidth : Px
    }



-- VIEW


view : State -> Options -> Html msg
view (State stateData) ({ grasses, windowDimension, imageWidth } as options) =
    let
        windowWidth =
            windowDimension |> Dimension.width

        ratio =
            Px.toFloat imageWidth / Px.toFloat windowWidth

        grassCount =
            List.length grasses

        columnWidth =
            windowWidth |> Px.divideBy grassCount
    in
    H.div
        [ HA.css
            [ width (pct 100)
            , height (pct 30)
            , position absolute
            , left (px 0)
            , bottom (px -15)
            , displayGrid
            , property "grid-template-columns"
                (grasses
                    |> List.map (always (columnWidth |> Px.toString))
                    |> String.join " "
                )
            ]
        ]
        (grasses |> List.map (.imageUrl >> viewGrass ratio stateData options))


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
