-- Main.elm


module Main exposing (main)

import Animation exposing (Animation)
import Browser
import Browser.Events
import Html exposing (Html)
import Millisecond exposing (Millisecond, millisecond)
import Svg as S exposing (..)
import Svg.Attributes as SA exposing (..)


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    { animations : List ( Millisecond, Animation )
    , animationState : Animation.State
    , timer : Millisecond
    }


pulseAnimation =
    ( millisecond 1000, Animation.pulse )


slideUpAnimation =
    ( millisecond 0, Animation.slideUp )


init : () -> ( Model, Cmd Msg )
init flags =
    let
        animations =
            [ pulseAnimation
            , slideUpAnimation
            ]
    in
    ( { animations = animations
      , animationState = Animation.run (millisecond 0) animations
      , timer = millisecond 0
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onAnimationFrameDelta (millisecond >> AnimationFrameTicked)


type Msg
    = AnimationFrameTicked Millisecond


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AnimationFrameTicked time ->
            let
                timer =
                    Millisecond.add model.timer time
            in
            ( { model
                | timer = timer
                , animationState = Animation.run timer model.animations
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    svg [ width "100%", height "100%", viewBox "0 0 1000 1000", SA.style "background: #ccc" ]
        [ Animation.toStyle model.animationState
        , dot [ slideUpAnimation, pulseAnimation ] { x = 200, y = 500 }
        , dot [] { x = 400, y = 500 }
        , dot [] { x = 600, y = 500 }
        , dot [] { x = 800, y = 500 }
        ]


dot : List ( Millisecond, Animation ) -> { x : Int, y : Int } -> Svg msg
dot animations { x, y } =
    g [ SA.style <| "transform: translate(" ++ String.fromInt x ++ "px," ++ String.fromInt y ++ "px)" ]
        [ circle
            [ cx "0"
            , cy "0"
            , r "50"
            , fill "red"
            , Animation.animate
                { duration = 2, iterationCount = Animation.once }
                animations
            ]
            []
        ]
