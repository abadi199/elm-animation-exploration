module Js.Animation exposing
    ( animation
    , encodeKeyframe
    , jsonToString
    , node
    , none
    , opacity
    , rotate
    , toString
    , translate
    , withOffset
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
import Js.Animation.Options as Options exposing (Options)
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
getCoordinate keyframe =
    case keyframe of
        Translate coordinate _ ->
            Just coordinate

        Opacity _ _ ->
            Nothing

        Rotate _ _ ->
            Nothing


rotate : Degree -> Keyframe
rotate degree =
    Rotate degree Offset.none


translate : { x : Px, y : Px } -> Keyframe
translate coord =
    Translate (coordinate coord) Offset.none


opacity : Percentage -> Keyframe
opacity level =
    Opacity level Offset.none


getOffset : Keyframe -> Offset
getOffset keyframe =
    case keyframe of
        Translate _ offset ->
            offset

        Opacity _ offset ->
            offset

        Rotate _ offset ->
            offset


none : H.Html msg -> H.Html msg
none html =
    html


animation : List Keyframe -> Options -> H.Html msg -> H.Html msg
animation keyframes options html =
    node keyframes options [] html


node : List Keyframe -> Options -> List (H.Attribute msg) -> H.Html msg -> H.Html msg
node keyframes options attributes html =
    H.node "elm-animation"
        (HA.attribute "animate" (toString keyframes options) :: attributes)
        [ html ]



--ENCODER


toString : List Keyframe -> Options -> String
toString keyframes options =
    JE.object
        [ ( "keyframes", JE.list encodeKeyframe keyframes )
        , ( "options", Options.encode options )
        ]
        |> jsonToString


jsonToString : JE.Value -> String
jsonToString =
    JE.encode 0


encodeKeyframe : Keyframe -> JE.Value
encodeKeyframe keyframe =
    case keyframe of
        Translate coordinate _ ->
            JE.object [ ( "transform", JE.string <| "translate" ++ Coordinate.toString coordinate ) ]

        Rotate rotation _ ->
            JE.object [ ( "transform", JE.string <| "rotate(" ++ Degree.toString rotation ++ ")" ) ]

        Opacity percentage _ ->
            JE.object [ ( "opacity", JE.string <| "rotate" ++ (percentage |> Percentage.toFloat |> String.fromFloat) ) ]



-- WITH MODIFIER


withOffset : Offset -> Keyframe -> Keyframe
withOffset offset keyframe =
    case keyframe of
        Translate a _ ->
            Translate a offset

        Opacity a _ ->
            Opacity a offset

        Rotate a _ ->
            Rotate a offset