module Fps exposing (Fps, fps, initial, update)

import Time exposing (Posix)


type Fps
    = Fps FpsCounter (List FpsCounter)


type alias FpsCounter =
    { beginTime : Posix, currentTime : Posix, frameCount : Int }


initial : Posix -> Fps
initial time =
    Fps { beginTime = time, currentTime = time, frameCount = 0 } []


update : Posix -> Fps -> Fps
update time (Fps fpsCounter past) =
    let
        timeElapsed =
            Time.posixToMillis time - Time.posixToMillis fpsCounter.beginTime
    in
    if timeElapsed > 1000 then
        Fps
            { beginTime = time
            , currentTime = time
            , frameCount = 0
            }
            (fpsCounter :: past)

    else
        Fps
            { fpsCounter
                | currentTime = time
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
