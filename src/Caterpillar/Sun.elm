module Caterpillar.Sun exposing (State, initialState, tick, view)

import Caterpillar.Shadow as Shadow
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
    { timer : Millisecond
    , rotation : Float
    }


initialState : State
initialState =
    State
        { timer = millisecond 0
        , rotation = 0
        }


type alias TickOptions a =
    { a
        | animationFrameDelta : Millisecond
        , rotationSpeed : Float
    }


tick : TickOptions a -> State -> State
tick { animationFrameDelta, rotationSpeed } (State stateData) =
    State
        { stateData
            | timer = stateData.timer |> Millisecond.add animationFrameDelta
            , rotation = stateData.rotation + (rotationSpeed * Millisecond.toFloat animationFrameDelta)
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
                , transform (rotate (deg stateData.rotation))
                , height (px 250)
                , width (px 250)
                , backgroundSize contain
                , Shadow.style showShadow
                ]
            ]
            []
        ]
