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
import Direction exposing (Direction)
import Easing exposing (Easing)
import Fill exposing (Fill)
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Encode as JE
import Millisecond exposing (Millisecond, millisecond)
import Offset exposing (Offset, offset)
import Percentage exposing (Percentage)
import Px exposing (Px, px)
import Second exposing (Second, second)


type Keyframe
    = Translate Coordinate Offset
    | Opacity Percentage Offset
    | Rotate Degree Offset


getCoordinate : Keyframe -> Maybe Coordinate
getCoordinate animation =
    case animation of
        Translate coordinate _ ->
            Just coordinate

        Opacity _ _ ->
            Nothing

        Rotate _ _ ->
            Nothing


rotate : Degree -> Keyframe
rotate degree =
    Rotate degree Offset.none


translate : Coordinate -> Keyframe
translate coordinate =
    Translate coordinate Offset.none


opacity : Percentage -> Keyframe
opacity level =
    Opacity level Offset.none


getOffset : Keyframe -> Offset
getOffset animation =
    case animation of
        Translate _ offset ->
            offset

        Opacity _ offset ->
            offset

        Rotate _ offset ->
            offset


type Options
    = Options OptionsData


type alias OptionsData =
    { delay : Millisecond
    , direction : Direction
    , duration : Millisecond
    , easing : Easing
    , endDelay : Millisecond
    , fill : Fill
    , iterationStart : NotImplemented
    , iterations : Count
    }


type NotImplemented
    = NotImplemented


node : List Keyframe -> Options -> List (H.Attribute msg) -> H.Html msg -> H.Html msg
node animations options attributes html =
    H.node "elm-animation"
        (HA.attribute "animate" (animationsToString animations) :: attributes)
        [ html ]



--ENCODER


animationsToString : List Keyframe -> String
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


encodeKeyframe : Keyframe -> JE.Value
encodeKeyframe animation =
    case animation of
        Translate coordinate _ ->
            JE.object [ ( "transform", JE.string <| "translate" ++ Coordinate.toString coordinate ) ]

        Rotate rotation _ ->
            JE.object [ ( "transform", JE.string <| "rotate(" ++ Degree.toString rotation ++ ")" ) ]

        Opacity percentage _ ->
            JE.object [ ( "opacity", JE.string <| "rotate" ++ (percentage |> Percentage.toFloat |> String.fromFloat) ) ]

        Delay _ delayedAnimation ->
            encodeKeyframe delayedAnimation

        Sequence animations ->
            Debug.todo "implement toKeyframeContent for sequence"



-- WITH MODIFIER


withOffset : Offset -> Keyframe -> Animation
withOffset offset animation =
    case animation of
        Translate a _ ->
            Translate a offset

        Opacity a _ ->
            Opacity a offset

        Rotate a _ ->
            Rotate a offset

        Delay a b ->
            Delay a (withOffset offset b)

        Sequence animations ->
            Sequence animations
