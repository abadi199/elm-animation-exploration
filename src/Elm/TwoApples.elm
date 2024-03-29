module Elm.TwoApples exposing (main)

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


speed : Float
speed =
    0.2


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view >> H.toUnstyled
        }


type alias Model =
    { flags : Flags
    , timeElapsed : Float
    , coordinate1 : Coordinate
    , coordinate2 : Coordinate
    }


type alias Flags =
    { apple : String }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { flags = flags
      , timeElapsed = 0
      , coordinate1 = coordinate { x = px 0, y = px 0 }
      , coordinate2 = coordinate { x = px 0, y = px 250 }
      }
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
            let
                timeElapsed =
                    model.timeElapsed + delta

                targetTime =
                    Px.toFloat targetX / speed
            in
            ( { model
                | timeElapsed = timeElapsed
                , coordinate1 =
                    if timeElapsed < targetTime then
                        move delta model.coordinate1

                    else
                        model.coordinate1
                , coordinate2 =
                    if timeElapsed > targetTime then
                        move delta model.coordinate2

                    else
                        model.coordinate2
              }
            , Cmd.none
            )


move : Float -> Coordinate -> Coordinate
move delta coordinate =
    let
        newCoordinate =
            coordinate |> Coordinate.addX (Px.px <| round (delta * speed))
    in
    if newCoordinate |> Coordinate.x |> Px.is (>) targetX then
        coordinate

    else
        newCoordinate



-- VIEW


view : Model -> Html Msg
view model =
    div [ HA.css [ Css.height (Css.px 500) ] ]
        [ apple model.coordinate1 model
        , apple model.coordinate2 model
        ]


apple : Coordinate -> Model -> Html msg
apple coordinate model =
    div
        [ HA.css
            [ Css.backgroundImage (Css.url model.flags.apple)
            , Css.width (Css.px 200)
            , Css.height (Css.px 200)
            , Css.backgroundSize (Css.pct 100)
            , Css.position Css.absolute
            , Css.left (coordinate |> Coordinate.x |> Px.toElmCss)
            , Css.top (coordinate |> Coordinate.y |> Px.toElmCss)
            , Shadow.style True
            ]
        ]
        []
