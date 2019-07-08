module Fps exposing (Fps, fps, initial, update, view)

import Css exposing (..)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as HA
import Millisecond exposing (Millisecond, millisecond)


type Fps
    = Fps FpsCounter (List FpsCounter)


type alias FpsCounter =
    { timer : Millisecond
    , frameCount : Int
    }


initial : Fps
initial =
    Fps { timer = millisecond 0, frameCount = 0 } []


update : Millisecond -> Fps -> Fps
update animationFrameDelta (Fps fpsCounter past) =
    let
        timeElapsed =
            fpsCounter.timer |> Millisecond.add animationFrameDelta
    in
    if Millisecond.toInt timeElapsed > 1000 then
        Fps
            { timer = millisecond 0
            , frameCount = 0
            }
            (fpsCounter :: past)

    else
        Fps
            { fpsCounter
                | timer = timeElapsed
                , frameCount = fpsCounter.frameCount + 1
            }
            past


fps : Fps -> Float
fps (Fps _ past) =
    let
        recentPast =
            past |> List.take 4
    in
    recentPast
        |> List.map (.frameCount >> toFloat)
        |> List.sum
        |> (\total -> total / toFloat (List.length recentPast))


view : { a | fps : Fps } -> Html msg
view data =
    let
        currentFps =
            data.fps |> fps
    in
    if isNaN currentFps then
        H.text ""

    else
        H.div
            [ HA.css
                [ position absolute
                , bottom (px 0)
                , fontSize (px 42)
                ]
            ]
            [ H.text "fps: ", H.text (currentFps |> String.fromFloat |> String.left 5) ]
