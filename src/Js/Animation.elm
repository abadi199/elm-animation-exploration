module Js.Animation exposing
    ( animationsToString
    , delay
    , encodeKeyframe
    , jsonToString
    , node
    , opacity
    , rotate
    , sequence
    , translate
    , withCount
    )

import Coordinate exposing (Coordinate, coordinate)
import Count exposing (Count)
import Degree exposing (Degree)
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Encode as JE
import Millisecond exposing (Millisecond, millisecond)
import Percentage exposing (Percentage)
import Px exposing (Px, px)
import Second exposing (Second, second)


type Animation
    = Move Coordinate Second Count
    | Opacity Percentage Second Count
    | Sequence (List Animation)
    | Delay Second Animation
    | Rotate Degree Second Count


getCoordinate : Animation -> Maybe Coordinate
getCoordinate animation =
    case animation of
        Move coordinate _ _ ->
            Just coordinate

        Opacity _ _ _ ->
            Nothing

        Sequence _ ->
            Nothing

        Delay _ delayedAnimation ->
            getCoordinate delayedAnimation

        Rotate _ _ _ ->
            Nothing


rotate : Degree -> Second -> Animation
rotate degree duration =
    Rotate degree duration Count.once


translate : Coordinate -> Second -> Animation
translate coordinate duration =
    Move coordinate duration Count.once


opacity : Percentage -> Second -> Animation
opacity level duration =
    Opacity level duration Count.once


delay : Second -> Animation -> Animation
delay =
    Delay


sequence : List Animation -> Animation
sequence =
    Sequence


getTotalDuration : List Animation -> Second
getTotalDuration animations =
    animations
        |> List.map (getDuration >> Maybe.withDefault (second 0) >> Second.toFloat)
        |> List.foldl (+) 0
        |> second


getTotalCount : List Animation -> Count
getTotalCount animations =
    Count.infinite


getCount : Animation -> Count
getCount animation =
    case animation of
        Move _ duration count ->
            count

        Opacity _ duration count ->
            count

        Delay delayDuration delayedAnimation ->
            getCount delayedAnimation

        Sequence _ ->
            Debug.todo "???"

        Rotate _ _ count ->
            count


getDuration : Animation -> Maybe Second
getDuration animation =
    case animation of
        Move _ duration _ ->
            Just duration

        Opacity _ duration _ ->
            Just duration

        Delay delayDuration delayedAnimation ->
            getDuration delayedAnimation
                |> Maybe.map (Second.add delayDuration)

        Sequence animations ->
            Just <| getTotalDuration animations

        Rotate _ duration _ ->
            Just duration


node : List Animation -> List (H.Attribute msg) -> H.Html msg -> H.Html msg
node animations attributes html =
    H.node "elm-animation"
        (HA.attribute "animate" (animationsToString animations) :: attributes)
        [ html ]



--ENCODER


animationsToString : List Animation -> String
animationsToString animations =
    let
        keyframes =
            JE.list encodeKeyframe animations

        totalDuration =
            getTotalDuration animations |> Millisecond.fromSecond |> Millisecond.toInt

        count =
            getTotalCount animations
                |> Count.encode

        options =
            JE.object [ ( "duration", JE.int totalDuration ), ( "iterations", count ) ]
    in
    JE.object [ ( "keyframes", keyframes ), ( "options", options ) ]
        |> jsonToString


jsonToString : JE.Value -> String
jsonToString =
    JE.encode 0


encodeKeyframe : Animation -> JE.Value
encodeKeyframe animation =
    case animation of
        Move coordinate _ _ ->
            JE.object [ ( "transform", JE.string <| "translate" ++ Coordinate.toString coordinate ) ]

        Rotate rotation _ _ ->
            JE.object [ ( "transform", JE.string <| "rotate(" ++ Degree.toString rotation ++ ")" ) ]

        Opacity percentage _ _ ->
            JE.object [ ( "opacity", JE.string <| "rotate" ++ (percentage |> Percentage.toFloat |> String.fromFloat) ) ]

        Delay _ delayedAnimation ->
            encodeKeyframe delayedAnimation

        Sequence animations ->
            Debug.todo "implement toKeyframeContent for sequence"



-- ITERATION COUNT


withCount : Count -> Animation -> Animation
withCount count animation =
    case animation of
        Move a b _ ->
            Move a b count

        Opacity a b _ ->
            Opacity a b count

        Delay a b ->
            Delay a (withCount count b)

        Sequence animations ->
            Sequence animations

        Rotate a b _ ->
            Rotate a b count
