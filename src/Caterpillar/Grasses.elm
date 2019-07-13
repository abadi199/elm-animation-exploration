module Caterpillar.Grasses exposing
    ( State
    , initialState
    , tick
    , view
    )

import Caterpillar.Shadow as Shadow
import Css exposing (..)
import Css.Grid as Grid exposing (displayGrid)
import Degree exposing (Degree, deg)
import Dimension exposing (Dimension)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as HA
import Millisecond exposing (Millisecond, millisecond)
import Px exposing (Px)


type State
    = State StateData


type alias StateData =
    { timer : Millisecond
    , rotations : List Degree
    }


initialState : Int -> State
initialState numberOfGrass =
    State
        { timer = millisecond 0
        , rotations = deg 20 |> List.repeat numberOfGrass
        }


tick : Millisecond -> State -> State
tick animationFrameDelta (State stateData) =
    State stateData


type alias Options =
    { grasses : List String
    , windowDimension : Dimension
    , loopDuration : Millisecond
    , showShadow : Bool
    , imageWidth : Px
    }


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

            -- , border3 (px 1) solid (rgba 255 0 0 1)
            , height (pct 30)
            , position absolute
            , left (px 0)
            , bottom (px -20)
            , displayGrid
            , property "grid-template-columns"
                (grasses
                    |> List.map (always (columnWidth |> Px.toString))
                    |> String.join " "
                )
            ]
        ]
        (grasses |> List.map (viewGrass ratio stateData options))


viewGrass : Float -> StateData -> Options -> String -> Html msg
viewGrass ratio stateData options imageUrl =
    H.div
        [ HA.css
            [ backgroundImage (url imageUrl)
            , property "background-position" "center bottom"
            , height (pct 100)
            , width (px (200 * ratio))
            , backgroundSize contain
            , backgroundRepeat noRepeat
            , Shadow.style options.showShadow
            , transform (rotate (Css.deg 20))

            -- , border3 (px 1) solid (rgba 0 0 0 1)
            ]
        ]
        []
