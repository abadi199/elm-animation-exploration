module Css.Animation exposing
    ( css
    , customCss
    , delay
    , iterationCount
    , none
    , opacity
    , sequence
    , toKeyframe
    , translate
    )

import Count exposing (Count)
import Html as H
import Html.Attributes as HA
import Html.Styled as HS
import Html.Styled.Attributes as HSA
import Murmur3
import Percentage exposing (Percentage)
import Px exposing (Px, px)
import Second exposing (Second, second)


type alias Html msg =
    List (HS.Attribute msg) -> List (HS.Html msg) -> HS.Html msg


type AnimatedHtml msg
    = Animated (List Animation) (Html msg)


type Animation
    = Move { x : Px, y : Px } Second Count
    | Opacity { from : Percentage, to : Percentage } Second Count
    | Sequence (List Animation)
    | Delay Second Animation


getCoordinate : Animation -> Maybe { x : Px, y : Px }
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


translate : { x : Px, y : Px } -> Second -> Animation
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


toKeyframes : List Animation -> HS.Html msg
toKeyframes animations =
    HS.node "style"
        []
        (animations
            |> List.map
                (\animation -> toKeyframe (hash animation) animation |> HS.text)
        )


toKeyframe : String -> Animation -> String
toKeyframe name animation =
    let
        nonDelayedKeyframe =
            "@keyframes "
                ++ name
                ++ " { "
                ++ toKeyframeContent animation
                ++ "}"
    in
    case animation of
        Move { x, y } _ _ ->
            nonDelayedKeyframe

        Opacity _ _ _ ->
            nonDelayedKeyframe

        Delay _ delayedAnimation ->
            toKeyframe name delayedAnimation

        Sequence animations ->
            nonDelayedKeyframe


toKeyframeContent : Animation -> String
toKeyframeContent animation =
    case animation of
        Move { x, y } _ _ ->
            "100% { transform: translate("
                ++ Px.toString x
                ++ ", "
                ++ Px.toString y
                ++ "); }"

        Opacity { from, to } _ _ ->
            "0% { opacity: "
                ++ (from |> Percentage.toFloat |> String.fromFloat)
                ++ "; }"
                ++ "100% { opacity: "
                ++ (to |> Percentage.toFloat |> String.fromFloat)
                ++ " }"

        Delay _ delayedAnimation ->
            toKeyframeContent delayedAnimation

        Sequence animations ->
            let
                totalDuration =
                    getTotalDuration animations

                initial =
                    ( "", { x = px 0, y = px 0 }, second 0 )

                ( keyframe, _, _ ) =
                    List.foldl (foldAnimation totalDuration) initial animations
            in
            keyframe


foldAnimation : Second -> Animation -> ( String, { x : Px, y : Px }, Second ) -> ( String, { x : Px, y : Px }, Second )
foldAnimation totalTime animation ( keyframe, coordinate, time ) =
    let
        nextCoordinate =
            getCoordinate animation
                |> Maybe.map (\{ x, y } -> { x = Px.add x coordinate.x, y = Px.add y coordinate.y })
                |> Maybe.withDefault coordinate

        nextTime =
            getDuration animation
                |> Maybe.map (Second.add time)
                |> Maybe.withDefault time

        percentage =
            Percentage.fromSecond { numerator = nextTime, denominator = totalTime }

        nextKeyframe =
            keyframe

        toCss animationForCss =
            case animationForCss of
                Move { x, y } _ _ ->
                    "transform: translate("
                        ++ Px.toString nextCoordinate.x
                        ++ ","
                        ++ Px.toString nextCoordinate.y
                        ++ ");"

                Opacity _ _ _ ->
                    "TODO: oldAnimation for opacity"

                Delay _ delayedAnimation ->
                    toCss delayedAnimation

                Sequence _ ->
                    "TODO: should we support nested sequence?"

        content =
            "{ " ++ toCss animation ++ " } "
    in
    ( nextKeyframe
        ++ Percentage.toString percentage
        ++ " "
        ++ content
    , nextCoordinate
    , nextTime
    )


toStyleAttribute : List Animation -> HS.Attribute msg
toStyleAttribute animations =
    HSA.style "animation"
        (animations |> List.map toAnimationStyle |> String.join ",")


toAnimationStyle : Animation -> String
toAnimationStyle animation =
    case animation of
        Move _ duration count ->
            hash animation
                ++ " "
                ++ Second.toString duration
                ++ " "
                ++ Count.toString count
                ++ " forwards"

        Opacity _ duration count ->
            hash animation
                ++ " "
                ++ Second.toString duration
                ++ " "
                ++ Count.toString count
                ++ " forwards"

        Delay delayDuration delayedAnimation ->
            hash delayedAnimation
                ++ " "
                ++ Second.toString (getDuration delayedAnimation |> Maybe.withDefault (second 0))
                ++ " "
                ++ Count.toString (getCount delayedAnimation)
                ++ " "
                ++ Second.toString delayDuration
                ++ " forwards"

        Sequence animations ->
            hash animation ++ " " ++ Second.toString (getTotalDuration animations) ++ " forwards"


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
            Count.once


getDuration : Animation -> Maybe Second
getDuration animation =
    case animation of
        Move _ duration _ ->
            Just duration

        Opacity _ duration _ ->
            Just duration

        Delay delayDuration delayedAnimation ->
            getDuration delayedAnimation

        Sequence animations ->
            Just <| getTotalDuration animations


hash : Animation -> String
hash animation =
    let
        seed =
            2019

        hashInt =
            Murmur3.hashString seed <| toKeyframeContent animation
    in
    case animation of
        Move _ _ _ ->
            "move" ++ String.fromInt hashInt

        Opacity _ _ _ ->
            "opacity" ++ String.fromInt hashInt

        Delay _ delayedAnimation ->
            hash delayedAnimation

        Sequence _ ->
            "sequence" ++ String.fromInt hashInt


none : List (HS.Attribute msg) -> List (HS.Html msg) -> Html msg -> HS.Html msg
none attributes children html =
    html attributes children


css : List Animation -> Html msg -> HS.Html msg
css animations html =
    customCss animations [] [] html


customCss : List Animation -> List (HS.Attribute msg) -> List (HS.Html msg) -> Html msg -> HS.Html msg
customCss animations attributes children html =
    html
        (toStyleAttribute animations :: attributes)
        (toKeyframes animations :: children)


iterationCount : Count -> Animation -> Animation
iterationCount count animation =
    case animation of
        Move a b _ ->
            Move a b count

        Opacity a b _ ->
            Opacity a b count

        Delay a b ->
            Delay a (iterationCount count b)

        Sequence animations ->
            Sequence animations
