module Caterpillar.Grasses exposing
    ( Grass
    , State
    , initialState
    , randomGenerator
    , tick
    , view
    )

import Caterpillar.Shadow as Shadow
import Css exposing (..)
import Css.Grid as Grid exposing (displayGrid)
import DegPerMs exposing (DegPerMs, degPerS)
import Degree exposing (Degree, deg)
import Dict exposing (Dict)
import Dimension exposing (Dimension)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as HA
import Millisecond exposing (Millisecond, millisecond)
import Px exposing (Px)
import Random



--STATE


maxRotation : Degree
maxRotation =
    deg 15


type State
    = State StateData


type alias Grass =
    { imageUrl : String
    , speed : DegPerMs
    }


type alias StateData =
    { timer : Millisecond
    , rotations : Dict String Degree
    }


initialState : List Grass -> State
initialState grasses =
    State
        { timer = millisecond 0
        , rotations = grasses |> List.map (\grass -> ( grass.imageUrl, deg 20 )) |> Dict.fromList
        }



-- TICK


type alias TickOptions a =
    { a
        | animationFrameDelta : Millisecond
        , speeds : Dict String DegPerMs
    }


tick : TickOptions a -> State -> State
tick { animationFrameDelta, speeds } (State stateData) =
    State
        { stateData
            | timer = stateData.timer |> Millisecond.add animationFrameDelta

            -- UPDATE rotations
            , rotations = stateData.rotations |> Dict.map (updateRotation speeds animationFrameDelta)
        }


updateRotation : Dict String DegPerMs -> Millisecond -> String -> Degree -> Degree
updateRotation speeds animationFrameDelta imageUrl rotation =
    Debug.todo "updateRotation"


type alias Options =
    { grasses : List Grass
    , windowDimension : Dimension
    , loopDuration : Millisecond
    , showShadow : Bool
    , imageWidth : Px
    }



-- VIEW


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
            , height (pct 30)
            , position absolute
            , left (px 0)
            , bottom (px -15)
            , displayGrid
            , property "grid-template-columns"
                (grasses
                    |> List.map (always (columnWidth |> Px.toString))
                    |> String.join " "
                )
            ]
        ]
        (grasses |> List.map (.imageUrl >> viewGrass ratio stateData options))


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
            , property "transform-origin" "50% 100%"
            , transform
                (stateData.rotations
                    |> Dict.get imageUrl
                    |> Maybe.map Degree.toFloat
                    |> Maybe.withDefault 0
                    |> Css.deg
                    |> rotate
                )
            ]
        ]
        []



-- RANDOM


randomGenerator : String -> Random.Generator Grass
randomGenerator imageUrl =
    let
        from =
            degPerS 10

        to =
            degPerS 20
    in
    Random.map (\speed -> { imageUrl = imageUrl, speed = speed })
        (DegPerMs.randomGenerator from to)
