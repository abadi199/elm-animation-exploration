module Elm.OneApple exposing (main)

import Browser
import Browser.Events
import Caterpillar.Shadow as Shadow
import Color exposing (Color)
import Coordinate exposing (Coordinate, coordinate)
import Count
import Css
import Html.Styled as H exposing (Html, div)
import Html.Styled.Attributes as HA
import Html.Styled.Events as HE
import Json.Decode as JD
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
    { flags : Flags, appleCoordinate : Coordinate }


type alias Box =
    { coordinate : Coordinate, color : Color }


type alias Flags =
    { apple : String }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { flags = flags, appleCoordinate = coordinate { x = px 0, y = px 0 } }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onAnimationFrameDelta AnimationFrameTick



-- MSG


type Msg
    = AnimationFrameTick Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AnimationFrameTick delta ->
            ( { model | appleCoordinate = move delta model.appleCoordinate }
            , Cmd.none
            )


move : Float -> Coordinate -> Coordinate
move delta coordinate =
    let
        newCoordinate =
            coordinate |> Coordinate.addX (Px.px <| round (delta * 0.2))
    in
    if newCoordinate |> Coordinate.x |> Px.is (>) targetX then
        coordinate |> Coordinate.setX (Px.px 0)

    else
        newCoordinate



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ apple model
        ]


apple : Model -> Html msg
apple model =
    div
        [ HA.css
            [ Css.backgroundImage (Css.url model.flags.apple)
            , Css.width (Css.px 200)
            , Css.height (Css.px 200)
            , Css.backgroundSize (Css.pct 100)
            , Css.position Css.absolute
            , Css.left (model.appleCoordinate |> Coordinate.x |> Px.toElmCss)
            , Css.top (model.appleCoordinate |> Coordinate.y |> Px.toElmCss)
            , Shadow.style True
            ]
        ]
        []
