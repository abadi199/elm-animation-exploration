module Caterpillar.Object exposing (State, initialState, tick, view)

import Caterpillar.Shadow as Shadow
import Coordinate exposing (Coordinate)
import Css exposing (..)
import Dimension exposing (Dimension)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as HA
import Millisecond exposing (Millisecond, millisecond)
import Px
import Time exposing (Posix)


type State
    = State StateData


type alias StateData =
    { timer : Millisecond }


initialState : State
initialState =
    State { timer = millisecond 0 }


type alias Options =
    { imageUrl : String
    , windowDimension : Dimension
    , loopDuration : Millisecond
    , dimension : Dimension
    , coordinate : Coordinate
    , showShadow : Bool
    }


tick : Millisecond -> State -> State
tick animationFrameDelta (State stateData) =
    State
        { stateData
            | timer =
                stateData.timer
                    |> Millisecond.add animationFrameDelta
        }


view : State -> Options -> Html msg
view (State stateData) { imageUrl, windowDimension, loopDuration, dimension, coordinate, showShadow } =
    let
        windowWidth =
            windowDimension
                |> Dimension.width
                |> Px.toInt
                |> toFloat

        timer =
            stateData.timer
                |> Millisecond.modBy (Millisecond.toInt loopDuration)
                |> Millisecond.toInt
                |> toFloat

        backgroundOffset =
            (timer * windowWidth) / Millisecond.toFloat loopDuration |> negate
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
            , left (px backgroundOffset)
            , Shadow.style showShadow
            ]
        ]
        []
