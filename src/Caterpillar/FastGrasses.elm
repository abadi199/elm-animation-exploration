module Caterpillar.FastGrasses exposing
    ( Grass
    , randomGenerator
    , view
    )

import Caterpillar.Shadow as Shadow
import Count
import Css exposing (..)
import Css.Grid as Grid exposing (displayGrid)
import Degree exposing (Degree, deg)
import Dict exposing (Dict)
import Dimension exposing (Dimension)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as HA
import Js.Animation as Animation
import Js.Animation.Options as Options
import Millisecond exposing (Millisecond, millisecond)
import Px exposing (Px)
import Random
import RotationSpeed exposing (RotationSpeed, degPerS)
import Speed exposing (Speed)



--STATE


maxRotation : Degree
maxRotation =
    deg 30


type alias Grass =
    { imageUrl : String
    , rotationSpeed : RotationSpeed
    }


type alias Options =
    { grasses : List Grass
    , grassAllUrl : String
    , windowDimension : Dimension
    , speed : Speed
    , showShadow : Bool
    , imageWidth : Px
    , isPaused : Bool
    }



-- VIEW


view : Options -> Html msg
view ({ speed, grasses, grassAllUrl, windowDimension, imageWidth, isPaused } as options) =
    let
        windowWidth =
            windowDimension |> Dimension.width

        ratio =
            Px.toFloat imageWidth / Px.toFloat windowWidth

        grassCount =
            List.length grasses

        columnWidth =
            windowWidth |> Px.divideBy grassCount

        gridColumns =
            grasses
                |> List.map (always (columnWidth |> Px.toString))
                |> String.join " "

        grassesView =
            grasses |> List.map (viewGrass ratio options)

        duration =
            speed |> Speed.toDuration windowWidth

        scaleFactor =
            (windowDimension |> Dimension.width |> Px.toFloat) / 1920

        grassesHeight =
            300 * scaleFactor |> px
    in
    Animation.styledNode
        [ Animation.translate { x = Px.px 0, y = Px.px 0 }
        , Animation.translate { x = windowWidth |> Px.map negate, y = Px.px 0 }
        ]
        (Options.default { duration = duration } |> Options.withIterations Count.infinite)
        [ if isPaused then
            HA.attribute "playback" "pause"

          else
            HA.attribute "playback" "play"
        ]
        (H.div
            [ HA.css
                [ width (pct 200)
                , height grassesHeight
                , position absolute
                , backgroundRepeat2 repeat noRepeat
                , backgroundSize (Px.toElmCss windowWidth)
                , bottom (px -15)
                , displayGrid
                , backgroundImage (url grassAllUrl)
                , Shadow.style options.showShadow
                , property "grid-template-columns" <|
                    gridColumns
                        ++ " "
                        ++ gridColumns
                        ++ " "
                        ++ gridColumns
                ]
            , HA.attribute "data-name" "grasses"
            ]
            (grassesView ++ grassesView)
        )


viewGrass : Float -> Options -> Grass -> Html msg
viewGrass ratio options { imageUrl, rotationSpeed } =
    let
        duration =
            rotationSpeed |> RotationSpeed.toDuration (maxRotation |> Degree.multiply 2)
    in
    Animation.styledNode
        [ Animation.rotate (Degree.deg 0)
        , Animation.rotate maxRotation
        , Animation.rotate (Degree.deg 0)
        ]
        (Options.default { duration = duration } |> Options.withIterations Count.infinite)
        []
    <|
        H.div
            [ HA.css
                [ backgroundImage (url imageUrl)
                , property "background-position" "center bottom"
                , height (pct 100)
                , width (px (200 * ratio))
                , backgroundSize contain
                , backgroundRepeat noRepeat
                , property "transform-origin" "50% 100%"
                , property "justify-self" "center"
                ]
            ]
            []



-- RANDOM


randomGenerator : String -> Random.Generator Grass
randomGenerator imageUrl =
    let
        from =
            degPerS 2

        to =
            degPerS 10
    in
    Random.map (\rotationSpeed -> { imageUrl = imageUrl, rotationSpeed = rotationSpeed })
        (RotationSpeed.randomGenerator from to)
