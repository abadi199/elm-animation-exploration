module Animation exposing
    ( Animation
    , IterationCount
    , State
    , animate
    , infinite
    , pulse
    , run
    , slideUp
    , toStyle
    )

import Html as H exposing (Html)
import Svg.Attributes as SA
import Time exposing (Posix)


type Animation
    = Animation { keyframes : ( String, List ( String, String ) ) }


type State
    = State


run : Posix -> List Animation -> State
run time animations =
    Debug.todo "run animation"


animate : { duration : Float, iterationCount : IterationCount } -> Animation -> H.Attribute msg
animate { duration, iterationCount } (Animation options) =
    SA.style <|
        "animation-name: "
            ++ (Tuple.first options.keyframes ++ ";")
            ++ "animation-duration: "
            ++ (String.fromFloat duration ++ "s;")
            ++ "animation-iteration-count: "
            ++ iterationCountToString iterationCount


type IterationCount
    = Infinite


infinite : IterationCount
infinite =
    Infinite


iterationCountToString iterationCount =
    case iterationCount of
        Infinite ->
            "infinite"


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
              , ( "100%", "transform: translate(-200px, 0)" )
              ]
            )
        }


toStyle : List Animation -> Html msg
toStyle animations =
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
