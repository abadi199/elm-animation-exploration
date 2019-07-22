module Caterpillar.Sun exposing (State, initialState, tick, view)

import Caterpillar.Shadow as Shadow
import Css exposing (..)
import Degree exposing (Degree)
import Dimension exposing (Dimension)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as HA
import Millisecond exposing (Millisecond, millisecond)
import Px
import RotationSpeed exposing (RotationSpeed)
import Time exposing (Posix)


sunDimension : Dimension
sunDimension =
    Dimension.dimension
        { width = Px.px 250
        , height = Px.px 250
        }


type State
    = State StateData


type alias StateData =
    { rotation : Degree
    }


initialState : State
initialState =
    State
        { rotation = Degree.deg 0
        }


type alias TickOptions a =
    { a
        | animationFrameDelta : Millisecond
        , rotationSpeed : RotationSpeed
    }


tick : TickOptions a -> State -> State
tick { animationFrameDelta, rotationSpeed } (State stateData) =
    State
        { stateData
            | rotation =
                stateData.rotation
                    |> Degree.add (rotationSpeed |> RotationSpeed.toDegree animationFrameDelta)
        }


type alias Options a =
    { a
        | sunUrl : String
        , sunRaysUrl : String
        , windowDimension : Dimension
        , showShadow : Bool
    }


view : State -> Options a -> Html msg
view (State stateData) { sunUrl, sunRaysUrl, windowDimension, showShadow } =
    let
        loopDuration =
            10000

        windowWidth =
            windowDimension
                |> Dimension.width
                |> Px.toInt
                |> toFloat

        scaleFactor =
            (windowDimension |> Dimension.width |> Px.toFloat) / 1920

        sunScaledDimension =
            sunDimension
                |> Dimension.scale scaleFactor

        bottomPosition =
            800 * scaleFactor |> px
    in
    H.div
        [ HA.css
            [ backgroundImage (url sunUrl)
            , backgroundRepeat2 noRepeat noRepeat
            , height (sunScaledDimension |> Dimension.height |> Px.toElmCss)
            , width (sunScaledDimension |> Dimension.width |> Px.toElmCss)
            , position absolute
            , left (vw 80)
            , bottom bottomPosition
            , backgroundSize contain
            , Shadow.style showShadow
            ]
        ]
        [ H.div
            [ HA.css
                [ backgroundImage (url sunRaysUrl)
                , backgroundRepeat2 noRepeat noRepeat
                , transform (rotate (deg <| Degree.toFloat stateData.rotation))
                , height (sunScaledDimension |> Dimension.height |> Px.toElmCss)
                , width (sunScaledDimension |> Dimension.width |> Px.toElmCss)
                , backgroundSize contain
                , Shadow.style showShadow
                ]
            ]
            []
        ]
