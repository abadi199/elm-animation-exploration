module Caterpillar.Caterpillar exposing (State, initialState, tick, view)

import Array exposing (Array)
import Caterpillar.Shadow as Shadow
import Css exposing (..)
import Dimension exposing (Dimension, dimension)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as HA
import Millisecond exposing (Millisecond, millisecond)
import Px
import PxPerMs exposing (PxPerMs)
import Time exposing (Posix)


type State
    = State StateData


type alias StateData =
    { timer : Millisecond
    , loopDuration : Millisecond
    }


initialState =
    State
        { timer = millisecond 0
        , loopDuration = millisecond 0
        }


caterpillarDimension : Dimension
caterpillarDimension =
    let
        width =
            600

        ratio =
            452 / 1000
    in
    dimension { width = Px.px width, height = Px.px (Basics.round <| width * ratio) }


backgroundPositions : Array Style
backgroundPositions =
    Array.fromList
        [ backgroundPosition2 zero zero
        , backgroundPosition2 (pct -100) zero
        , backgroundPosition2 (pct -200) zero
        , backgroundPosition2 zero (pct -100)
        , backgroundPosition2 (pct -100) (pct -100)
        , backgroundPosition2 (pct -200) (pct -100)
        , backgroundPosition2 zero (pct -200)
        , backgroundPosition2 (pct -100) (pct -200)
        , backgroundPosition2 (pct -200) (pct -200)
        , backgroundPosition2 zero (pct -300)
        ]


calculateBackgroundPositionIndex : State -> Int
calculateBackgroundPositionIndex (State stateData) =
    let
        numberOfFrames =
            Array.length backgroundPositions
    in
    Millisecond.toInt stateData.timer * numberOfFrames // Millisecond.toInt stateData.loopDuration


type alias TickOptions =
    { animationFrameDelta : Millisecond
    , loopDuration : Millisecond
    , windowDimension : Dimension
    , speed : PxPerMs
    }


tick : TickOptions -> State -> State
tick { animationFrameDelta, loopDuration } (State stateData) =
    State
        { stateData
            | loopDuration = loopDuration
            , timer =
                stateData.timer
                    |> Millisecond.add animationFrameDelta
                    |> Millisecond.modBy loopDuration
        }


view : { caterpillar : String, windowDimension : Dimension, showShadow : Bool, state : State } -> Html msg
view { caterpillar, windowDimension, showShadow, state } =
    let
        positionIndex =
            calculateBackgroundPositionIndex state
    in
    H.div
        [ HA.css
            [ position absolute
            , backgroundImage (url caterpillar)
            , backgroundSize (pct 300)
            , backgroundPositions |> Array.get positionIndex |> Maybe.withDefault (backgroundPosition2 zero zero)
            , left
                (windowDimension
                    |> Dimension.width
                    |> Px.divideBy 2
                    |> Px.add (Px.px ((caterpillarDimension |> Dimension.width |> Px.toInt |> toFloat |> negate |> Basics.round) // 2))
                    |> Px.toElmCss
                )
            , bottom (px 200)
            , width (caterpillarDimension |> Dimension.width |> Px.toElmCss)
            , height (caterpillarDimension |> Dimension.height |> Px.toElmCss)
            , Shadow.style showShadow
            ]
        ]
        []
