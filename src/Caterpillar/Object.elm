module Caterpillar.Object exposing (State, initialState, tick, view)

import Caterpillar.Shadow as Shadow
import Coordinate exposing (Coordinate)
import Css exposing (..)
import Dimension exposing (Dimension)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as HA
import Millisecond exposing (Millisecond, millisecond)
import Px exposing (Px)
import PxPerMs exposing (PxPerMs)
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


type alias Options =
    { imageUrl : String
    , windowDimension : Dimension
    , loopDuration : Millisecond
    , dimension : Dimension
    , coordinate : Coordinate
    , showShadow : Bool
    }


type alias TickOptions =
    { animationFrameDelta : Millisecond
    , loopDuration : Millisecond
    , windowDimension : Dimension
    , speed : PxPerMs
    }


tick : TickOptions -> State -> State
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
                            |> Px.add (speed |> PxPerMs.toPx animationFrameDelta)
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


view : State -> Options -> Html msg
view (State stateData) { imageUrl, windowDimension, dimension, coordinate, showShadow } =
    let
        windowWidth =
            windowDimension
                |> Dimension.width
                |> Px.toInt
                |> toFloat

        timer =
            stateData.timer
                |> Millisecond.toInt
                |> toFloat
    in
    H.div
        [ HA.css
            [ backgroundImage (url imageUrl)
            , backgroundSize (px windowWidth)
            , backgroundRepeat2 repeat noRepeat
            , height (dimension |> Dimension.height |> Px.toElmCss)
            , width (dimension |> Dimension.width |> Px.multiply 2 |> Px.toElmCss)
            , position absolute
            , top (coordinate |> Coordinate.y |> Px.toElmCss)
            , left (stateData.positionX |> Px.toElmCss)
            , Shadow.style showShadow
            ]
        ]
        []
