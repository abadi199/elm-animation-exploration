module Css.Animation exposing
    ( delay
    , none
    , sequence
    , toKeyframe
    , translate
    , withAnimation
    )

import Html as H
import Html.Attributes as HA
import Murmur3
import Percentage
import Px exposing (Px, px)
import Second exposing (Second, second)


type alias Html msg =
    List (H.Attribute msg) -> List (H.Html msg) -> H.Html msg


type AnimatedHtml msg
    = Animated (List Animation) (Html msg)


type Animation
    = Move { x : Px, y : Px } Second
    | Sequence (List Animation)
    | Delay Second Animation


getCoordinate : Animation -> { x : Px, y : Px }
getCoordinate animation =
    case animation of
        Move coordinate _ ->
            coordinate

        Sequence _ ->
            Debug.todo "sequence"

        Delay _ delayedAnimation ->
            getCoordinate delayedAnimation


translate : { x : Px, y : Px } -> Second -> Animation
translate distance duration =
    Move distance duration


delay : Second -> Animation -> Animation
delay =
    Delay


sequence : List Animation -> Animation
sequence =
    Sequence


toStyleNode : List Animation -> H.Html msg
toStyleNode animations =
    H.node "style"
        []
        (animations
            |> List.map
                (\animation -> toKeyframe (hash animation) animation |> H.text)
        )


toKeyframe : String -> Animation -> String
toKeyframe name animation =
    case animation of
        Move { x, y } _ ->
            "@keyframes "
                ++ name
                ++ "{"
                ++ toKeyframeContent animation
                ++ "}"

        Delay _ delayedAnimation ->
            toKeyframe name delayedAnimation

        Sequence animations ->
            "@keyframes " ++ name ++ " { " ++ toKeyframeContent animation ++ "}"


toKeyframeContent : Animation -> String
toKeyframeContent animation =
    case animation of
        Move { x, y } _ ->
            "0% { transform: translate(0, 0); }"
                ++ "100% { transform: translate("
                ++ Px.toString x
                ++ ", "
                ++ Px.toString y
                ++ "); }"

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
                |> (\{ x, y } -> { x = Px.add x coordinate.x, y = Px.add y coordinate.y })

        nextTime =
            Second.add time (getDuration animation)

        percentage =
            Percentage.fromSecond { numerator = nextTime, denominator = totalTime }

        nextKeyframe =
            keyframe

        toCss animationForCss =
            case animationForCss of
                Move { x, y } _ ->
                    "transform: translate("
                        ++ Px.toString nextCoordinate.x
                        ++ ","
                        ++ Px.toString nextCoordinate.y
                        ++ ");"

                Delay _ delayedAnimation ->
                    toCss delayedAnimation

                Sequence _ ->
                    Debug.todo "nested sequence is not supported"

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


toStyleAttribute : List Animation -> H.Attribute msg
toStyleAttribute animations =
    HA.style "animation"
        (animations |> List.map toAnimationStyle |> String.join ",")


toAnimationStyle : Animation -> String
toAnimationStyle animation =
    case animation of
        Move _ duration ->
            hash animation ++ " " ++ Second.toString duration ++ " forwards"

        Delay delayDuration delayedAnimation ->
            hash delayedAnimation
                ++ " "
                ++ Second.toString (getDuration delayedAnimation)
                ++ " "
                ++ Second.toString delayDuration
                ++ " forwards"

        Sequence animations ->
            hash animation ++ " " ++ Second.toString (getTotalDuration animations) ++ " forwards"


getTotalDuration : List Animation -> Second
getTotalDuration animations =
    animations
        |> List.map (getDuration >> Second.toFloat)
        |> List.foldl (+) 0
        |> second


getDuration : Animation -> Second
getDuration animation =
    case animation of
        Move _ duration ->
            duration

        Delay delayDuration delayedAnimation ->
            Second.add delayDuration (getDuration delayedAnimation)

        Sequence animations ->
            getTotalDuration animations


hash : Animation -> String
hash animation =
    let
        seed =
            2019
    in
    case animation of
        Move _ _ ->
            "move" ++ String.fromInt (Murmur3.hashString seed <| toKeyframeContent animation)

        Delay _ delayedAnimation ->
            hash delayedAnimation

        Sequence _ ->
            "sequence" ++ String.fromInt (Murmur3.hashString seed <| toKeyframeContent animation)


none : List (H.Attribute msg) -> List (H.Html msg) -> Html msg -> H.Html msg
none attributes children html =
    html attributes children


withAnimation : List Animation -> List (H.Attribute msg) -> List (H.Html msg) -> Html msg -> H.Html msg
withAnimation animations attributes children html =
    html
        (toStyleAttribute animations :: attributes)
        (toStyleNode animations :: children)
