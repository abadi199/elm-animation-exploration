module Caterpillar.Caterpillar exposing (State, initialState, tick, view)

import Array exposing (Array)
import Caterpillar.Shadow as Shadow
import Css exposing (..)
import Dimension exposing (Dimension, dimension)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as HA
import Millisecond exposing (Millisecond, millisecond)
import Px
import Speed exposing (Speed)


type State
    = State StateData


type alias StateData =
    { timer : Millisecond
    , loopDuration : Millisecond
    }


initialState : State
initialState =
    State
        { timer = millisecond 0
        , loopDuration = millisecond 0
        }


caterpillarDimension : Dimension
caterpillarDimension =
    let
        width =
            500

        ratio =
            452 / 1000
    in
    dimension { width = Px.px width, height = Px.px (Basics.round <| width * ratio) }


backgroundPositions : Array Style
backgroundPositions =
    Array.fromList
        [ backgroundPosition2 zero zero
        , backgroundPosition2 zero (pct -100)
        , backgroundPosition2 zero (pct -200)
        , backgroundPosition2 zero (pct -300)
        , backgroundPosition2 zero (pct -400)
        , backgroundPosition2 zero (pct -500)
        , backgroundPosition2 zero (pct -600)
        , backgroundPosition2 zero (pct -700)
        , backgroundPosition2 zero (pct -800)
        , backgroundPosition2 zero (pct -900)
        , backgroundPosition2 zero (pct -1000)
        ]


calculateBackgroundPositionIndex : State -> Int
calculateBackgroundPositionIndex (State stateData) =
    let
        numberOfFrames =
            Array.length backgroundPositions
    in
    Millisecond.toInt stateData.timer * numberOfFrames // Millisecond.toInt stateData.loopDuration


type alias TickOptions a =
    { a
        | animationFrameDelta : Millisecond
        , loopDuration : Millisecond
        , windowDimension : Dimension
        , speed : Speed
    }


tick : TickOptions a -> State -> State
tick { animationFrameDelta, loopDuration } (State stateData) =
    State
        { stateData
            | loopDuration = loopDuration
            , timer =
                stateData.timer
                    |> Millisecond.add animationFrameDelta
                    |> Millisecond.modBy loopDuration
        }


view : { isHappy : Bool, smile : String, frown : String, windowDimension : Dimension, showShadow : Bool, state : State } -> Html msg
view { isHappy, smile, frown, windowDimension, showShadow, state } =
    let
        positionIndex =
            calculateBackgroundPositionIndex state

        scaleFactor =
            (windowDimension |> Dimension.width |> Px.toFloat) / 1920

        caterpillarScaledDimension =
            caterpillarDimension
                |> Dimension.scale scaleFactor

        bottomPosition =
            300 * scaleFactor |> px

        caterpillar visible image =
            H.div
                [ HA.css
                    [ backgroundImage (url image)
                    , backgroundSize (pct 100)
                    , backgroundPositions |> Array.get positionIndex |> Maybe.withDefault (backgroundPosition2 zero zero)
                    , width (pct 100)
                    , height (pct 100)
                    , Shadow.style showShadow
                    , position absolute
                    , top (px 0)
                    , left (px 0)
                    , property "transition" "opacity 1s"
                    , if visible then
                        opacity (num 1)

                      else
                        opacity (num 0)
                    ]
                , HA.attribute "data-name" "caterpillar"
                ]
                []
    in
    H.div
        [ HA.css
            [ position absolute
            , left
                (windowDimension
                    |> Dimension.width
                    |> Px.divideBy 2
                    |> Px.add
                        (Px.px
                            ((caterpillarScaledDimension
                                |> Dimension.width
                                |> Px.toInt
                                |> toFloat
                                |> negate
                                |> Basics.round
                             )
                                // 2
                            )
                        )
                    |> Px.toElmCss
                )
            , bottom bottomPosition
            , width (caterpillarScaledDimension |> Dimension.width |> Px.toElmCss)
            , height (caterpillarScaledDimension |> Dimension.height |> Px.toElmCss)
            ]
        , HA.attribute "data-name" "caterpillar"
        ]
        [ caterpillar (not isHappy) frown, caterpillar isHappy smile ]
