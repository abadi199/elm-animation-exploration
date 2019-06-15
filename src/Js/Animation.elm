module Js.Animation exposing
    ( animationsToString
    , delay
    , encode
    , node
    , none
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
import Percentage exposing (Percentage)
import Px exposing (Px, px)
import Second exposing (Second, second)


type alias Html msg =
    List (H.Attribute msg) -> List (H.Html msg) -> H.Html msg


type AnimatedHtml msg
    = Animated (List Animation) (Html msg)


type Animation
    = Move Coordinate Second Count
    | Opacity { from : Percentage, to : Percentage } Second Count
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


opacity : { from : Percentage, to : Percentage } -> Second -> Animation
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


none : List (H.Attribute msg) -> List (H.Html msg) -> Html msg -> H.Html msg
none attributes children html =
    html attributes children


node : List Animation -> List (H.Attribute msg) -> H.Html msg -> H.Html msg
node animations attributes html =
    H.node "elm-animation"
        (HA.attribute "animate" (animationsToString animations) :: attributes)
        [ html ]


encode : Animation -> JE.Value
encode animation =
    JE.object [ ( "keyframes", JE.string "" ) ]


animationsToString : List Animation -> String
animationsToString animations =
    "{\"keyframes\":[{\"transform\":\"rotate(0deg)\"},{\"transform\":\"rotate(360deg)\"}],\"options\":{\"duration\":3000,\"iterations\":\"Infinity\"}}"


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
