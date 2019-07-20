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
    in
    H.div
        [ HA.css
            [ backgroundImage (url sunUrl)
            , backgroundRepeat2 noRepeat noRepeat
            , height (px 250)
            , width (px 250)
            , position absolute
            , left (vw 80)
            , top (vh 5)
            , backgroundSize contain
            , Shadow.style showShadow
            ]
        ]
        [ H.div
            [ HA.css
                [ backgroundImage (url sunRaysUrl)
                , backgroundRepeat2 noRepeat noRepeat
                , transform (rotate (deg <| Degree.toFloat stateData.rotation))
                , height (px 250)
                , width (px 250)
                , backgroundSize contain
                , Shadow.style showShadow
                ]
            ]
            []
        ]
