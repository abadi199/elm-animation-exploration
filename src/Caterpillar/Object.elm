module Caterpillar.Object exposing (State, continuousTick, initialState, tick, view)

import Caterpillar.Shadow as Shadow
import Coordinate exposing (Coordinate)
import Css exposing (..)
import Dimension exposing (Dimension)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as HA
import Millisecond exposing (Millisecond, millisecond)
import Px exposing (Px)
import Speed exposing (Speed)
import Time exposing (Posix)


type State
    = State StateData


type alias StateData =
    { timer : Millisecond
    , positionX : Px
    }


initialState : State
initialState =
    State
        { timer = millisecond 0
        , positionX = Px.px 0
        }


type alias TickOptions a =
    { a
        | animationFrameDelta : Millisecond
        , loopDuration : Millisecond
        , windowDimension : Dimension
        , speed : Speed
    }


continuousTick : TickOptions a -> State -> State
continuousTick { animationFrameDelta, windowDimension, speed } (State stateData) =
    let
        positionX =
            let
                windowWidth =
                    windowDimension |> Dimension.width

                newPositionX =
                    stateData.positionX
                        |> Px.add (speed |> Speed.toDistance animationFrameDelta)
            in
            if newPositionX |> Px.is (<) (Px.map negate windowWidth) then
                Px.px 0

            else if newPositionX |> Px.is (>) windowWidth then
                Px.px 0

            else
                newPositionX
    in
    State
        { stateData
            | positionX = positionX
        }


tick : TickOptions a -> State -> State
tick { animationFrameDelta, loopDuration, windowDimension, speed } (State stateData) =
    let
        timer =
            stateData.timer
                |> Millisecond.add animationFrameDelta
                |> Millisecond.modBy loopDuration

        halfLoop =
            loopDuration |> Millisecond.multiply 0.6

        positionX =
            if timer |> Millisecond.is (>) halfLoop then
                let
                    windowWidth =
                        windowDimension |> Dimension.width

                    newPositionX =
                        stateData.positionX
                            |> Px.add (speed |> Speed.toDistance animationFrameDelta)
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
            | timer = timer
            , positionX = positionX
        }


type alias Options =
    { imageUrl : String
    , windowDimension : Dimension
    , loopDuration : Millisecond
    , dimension : Dimension
    , coordinate : Coordinate
    , showShadow : Bool
    , name : String
    }


view : State -> Options -> Html msg
view (State stateData) { name, imageUrl, windowDimension, dimension, coordinate, showShadow } =
    let
        windowWidth =
            windowDimension
                |> Dimension.width

        scaleFactor =
            (windowDimension |> Dimension.width |> Px.toFloat) / 1920

        objectHeight =
            dimension |> Dimension.height |> Px.scale scaleFactor

        bottomCoordinate =
            coordinate
                |> Coordinate.y
                |> Px.scale scaleFactor
    in
    H.div
        [ HA.css
            [ backgroundImage (url imageUrl)
            , backgroundSize (windowWidth |> Px.toElmCss)
            , backgroundRepeat2 repeat noRepeat
            , height (objectHeight |> Px.toElmCss)
            , width (dimension |> Dimension.width |> Px.multiply 3 |> Px.toElmCss)
            , position absolute
            , bottom (bottomCoordinate |> Px.toElmCss)
            , left (stateData.positionX |> Px.add (Px.map negate windowWidth) |> Px.toElmCss)
            , Shadow.style showShadow
            ]
        , HA.attribute "data-name" name
        ]
        []
