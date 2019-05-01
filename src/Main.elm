-- Main.elm


module Main exposing (main)

import Animation exposing (Animation)
import Browser
import Html exposing (Html)
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
    { animations : List Animation }


pulseAnimation =
    Animation.pulse


slideUpAnimation =
    Animation.slideUp


init : () -> ( Model, Cmd Msg )
init flags =
    let animations = [ (0, pulseAnimation)
            , (1000, slideUpAnimation)
            ]
    in 
    ( { animations = animations
      , animationState = Animation.run animations
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type Msg
    = DeleteMe


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DeleteMe ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    svg [ width "100%", height "100%", viewBox "0 0 1000 1000", SA.style "background: #ccc" ]
        [ Animation.toStyle model.animations
        , dot { x = 200, y = 500 }
        , dot { x = 400, y = 500 }
        , dot { x = 600, y = 500 }
        , dot { x = 800, y = 500 }
        ]


dot : { x : Int, y : Int } -> Svg msg
dot { x, y } =
    g [ SA.style <| "transform: translate(" ++ String.fromInt x ++ "px," ++ String.fromInt y ++ "px)" ]
        [ circle
            [ cx "0"
            , cy "0"
            , r "50"
            , fill "red"
            , Animation.animate
                { duration = 2, iterationCount = Animation.infinite }
                pulseAnimation
            ]
            []
        ]
