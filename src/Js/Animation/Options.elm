module Js.Animation.Options exposing
    ( Options
    , default
    , encode
    , withIterations
    )

import Count exposing (Count)
import Degree exposing (Degree)
import Direction exposing (Direction)
import Easing exposing (Easing)
import Fill exposing (Fill)
import Json.Encode as JE
import Millisecond exposing (Millisecond, millisecond)
import Offset exposing (Offset, offset)


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


default : { duration : Millisecond } -> Options
default { duration } =
    Options
        { duration = duration
        , delay = millisecond 0
        , direction = Direction.default
        , easing = Easing.default
        , endDelay = millisecond 0
        , fill = Fill.default
        , iterationStart = NotImplemented
        , iterations = Count.default
        }


withIterations : Count -> Options -> Options
withIterations count (Options options) =
    Options { options | iterations = count }


encode : Options -> JE.Value
encode (Options options) =
    JE.object
        [ ( "duration", Millisecond.encode options.duration )
        , ( "iterations", Count.encode options.iterations )
        ]
