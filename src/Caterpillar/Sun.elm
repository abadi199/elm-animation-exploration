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


type alias Options =
    { smile : String
    , frown : String
    , isHappy : Bool
    , sunRaysUrl : String
    , windowDimension : Dimension
    , showShadow : Bool
    }


view : State -> Options -> Html msg
view (State stateData) { isHappy, smile, frown, sunRaysUrl, windowDimension, showShadow } =
    let
        scaleFactor =
            (windowDimension |> Dimension.width |> Px.toFloat) / 1920

        sunScaledDimension =
            sunDimension
                |> Dimension.scale scaleFactor

        bottomPosition =
            800 * scaleFactor |> px

        sun visible image =
            H.div
                [ HA.css
                    [ backgroundImage (url image)
                    , backgroundRepeat2 noRepeat noRepeat
                    , height (sunScaledDimension |> Dimension.height |> Px.toElmCss)
                    , width (sunScaledDimension |> Dimension.width |> Px.toElmCss)
                    , position absolute
                    , left (px 0)
                    , width (pct 100)
                    , height (pct 100)
                    , top (px 0)
                    , backgroundSize contain
                    , Shadow.style showShadow
                    , property "transition" "opacity 1s"
                    , if visible then
                        opacity (num 1)

                      else
                        opacity (num 0)
                    ]
                ]
                []
    in
    H.div
        [ HA.css
            [ height (sunScaledDimension |> Dimension.height |> Px.toElmCss)
            , width (sunScaledDimension |> Dimension.width |> Px.toElmCss)
            , position absolute
            , left (vw 80)
            , bottom bottomPosition
            ]
        ]
        [ sun (not isHappy) frown
        , sun isHappy smile
        , H.div
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
