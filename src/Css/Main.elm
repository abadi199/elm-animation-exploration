module Css.Main exposing (main)

import Browser
import Coordinate exposing (Coordinate, coordinate)
import Count
import Css
import Css.Animation as Animation
import Html.Styled as H exposing (Html, div)
import Html.Styled.Attributes as HA
import Html.Styled.Events as HE
import Percentage exposing (percentage)
import Px exposing (px)
import Second exposing (second)
import Shadow as Shadow


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view >> H.toUnstyled
        }


type alias Model =
    { flags : Flags
    }


type alias Flags =
    { apple : String }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { flags = flags
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    let
        apple coordinate attributes children =
            div
                (HA.css
                    [ Css.backgroundImage (Css.url model.flags.apple)
                    , Css.width (Css.px 200)
                    , Css.height (Css.px 200)
                    , Css.backgroundSize (Css.pct 100)
                    , Css.position Css.absolute
                    , Css.left (coordinate |> Coordinate.x |> Px.toElmCss)
                    , Css.top (coordinate |> Coordinate.y |> Px.toElmCss)
                    , Shadow.style True
                    ]
                    :: attributes
                )
                children
    in
    div [ HA.css [ Css.position Css.relative ] ]
        [ apple (coordinate { x = px 0, y = px -250 })
            |> Animation.css
                [ Animation.sequence
                    [ Animation.translate { x = px 500, y = px 0 } (second 2)

                    -- , Animation.translate { x = px 0, y = px 300 } (second 2)
                    ]
                ]
        , apple (coordinate { x = px 0, y = px 50 })
            |> Animation.css
                [ Animation.translate { x = px 500, y = px 0 } (second 2)
                    |> Animation.delay (second 2)
                , Animation.opacity { from = percentage 100, to = percentage 0 } (second 2) |> Animation.delay (second 2)
                ]
        ]
