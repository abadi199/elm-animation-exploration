module Animation exposing
    ( Animation
    , IterationCount
    , State
    , animate
    , infinite
    , multiple
    , once
    , pulse
    , run
    , slideUp
    , toStyle
    )

import Html as H exposing (Html)
import Millisecond exposing (Millisecond, millisecond)
import Svg.Attributes as SA


type Animation
    = Animation { keyframes : ( String, List ( String, String ) ) }


type State
    = State (List Animation)


run : Millisecond -> List ( Millisecond, Animation ) -> State
run current animations =
    animations
        |> List.filterMap
            (\( time, animation ) ->
                if Millisecond.isAfter current time then
                    Just animation

                else
                    Nothing
            )
        |> State


animate : { duration : Float, iterationCount : IterationCount } -> List ( Millisecond, Animation ) -> H.Attribute msg
animate { duration, iterationCount } animations =
    let
        toAnimationStyle ( time, Animation options ) =
            (Tuple.first options.keyframes ++ " ")
                ++ (String.fromFloat duration ++ "s ")
                ++ (iterationCountToString iterationCount ++ " ")
    in
    animations
        |> List.map toAnimationStyle
        |> String.join ","
        |> (\style -> "animation: " ++ style ++ "; animation-fill-mode: forwards;")
        |> SA.style


type IterationCount
    = Infinite
    | Multiple Float
    | Once


infinite : IterationCount
infinite =
    Infinite


once : IterationCount
once =
    Once


multiple : Float -> IterationCount
multiple =
    Multiple


iterationCountToString iterationCount =
    case iterationCount of
        Infinite ->
            "infinite"

        Once ->
            "1"

        Multiple count ->
            String.fromFloat count


pulse : Animation
pulse =
    Animation
        { keyframes =
            ( "pulse"
            , [ ( "0%", "transform: scale(1,1); opacity: 1" )
              , ( "100%", "transform: scale(2,2); opacity: 0.025" )
              ]
            )
        }


slideUp : Animation
slideUp =
    Animation
        { keyframes =
            ( "slideUp"
            , [ ( "0%", "transform: translate(0px, 0)" )
              , ( "100%", "transform: translate(0px, -500px)" )
              ]
            )
        }



-- toStyle : List Animation -> Html msg
-- toStyle animations =
--     H.node "style"
--         []
--         [ H.text (animations |> List.map toKeyframe |> String.join "\n") ]


toStyle : State -> Html msg
toStyle (State animations) =
    H.node "style"
        []
        [ H.text (animations |> List.map toKeyframe |> String.join "\n") ]


toKeyframe : Animation -> String
toKeyframe (Animation { keyframes }) =
    let
        ( name, timelines ) =
            keyframes

        toTimelineStyle ( time, content ) =
            time ++ " {" ++ content ++ "}"
    in
    "@keyframes "
        ++ name
        ++ "{"
        ++ (timelines |> List.map toTimelineStyle |> String.join "\n")
        ++ "}"
