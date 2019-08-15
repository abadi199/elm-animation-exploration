module Js.TwoApples exposing (main)

import Browser
import Browser.Events
import Caterpillar.Shadow as Shadow
import Color exposing (Color)
import Coordinate exposing (Coordinate, coordinate)
import Count
import Css
import Fill
import Html.Styled as H exposing (Html, div)
import Html.Styled.Attributes as HA
import Html.Styled.Events as HE
import Js.Animation as Animation
import Js.Animation.Events as Events
import Js.Animation.Options as Options
import Json.Decode as JD
import Millisecond exposing (millisecond)
import Percentage
import Px exposing (Px, px)
import Random
import Second exposing (second)
import Time exposing (Posix)


targetX : Px
targetX =
    Px.px 500


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view >> H.toUnstyled
        }


type alias Model =
    { flags : Flags
    , animationState : AnimationState
    }


type AnimationState
    = FirstAppleMoving
    | SecondAppleMoving


type alias Flags =
    { apple : String }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { flags = flags
      , animationState = FirstAppleMoving
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MSG


type Msg
    = FirstAppleAnimationFinish


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FirstAppleAnimationFinish ->
            ( { model | animationState = SecondAppleMoving }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    let
        options =
            Options.default { duration = millisecond 2000 }
                |> Options.withFill Fill.forwards
    in
    div [ HA.css [ Css.height (Css.px 400) ] ]
        [ Animation.styledNode
            [ Animation.translate { x = px 0, y = px 0 }
            , Animation.translate { x = targetX, y = px 0 }
            ]
            options
            [ Events.onFinish FirstAppleAnimationFinish ]
            (apple model)
        , Animation.styledNode
            (case model.animationState of
                FirstAppleMoving ->
                    []

                SecondAppleMoving ->
                    [ Animation.translate { x = px 0, y = px 0 }
                    , Animation.translate { x = targetX, y = px 0 }
                    ]
            )
            options
            []
            (apple model)
        ]


apple : Model -> Html msg
apple model =
    div
        [ HA.css
            [ Css.backgroundImage (Css.url model.flags.apple)
            , Css.width (Css.px 200)
            , Css.height (Css.px 200)
            , Css.backgroundSize (Css.pct 100)
            , Shadow.style True
            ]
        ]
        []
