module Elm.OneApple exposing (main)

import Browser
import Browser.Events
import Caterpillar.Shadow as Shadow
import Coordinate exposing (Coordinate, coordinate)
import Css
import Html.Styled as H exposing (Html, div)
import Html.Styled.Attributes as HA
import Px exposing (Px, px)


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
    , coordinate : Coordinate
    , timeElapsed : Float
    }


type alias Flags =
    { apple : String }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { flags = flags
      , coordinate = coordinate { x = px 0, y = px 0 }
      , timeElapsed = 0
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta AnimationFrameTick



-- MSG


type Msg
    = AnimationFrameTick Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AnimationFrameTick delta ->
            ( { model
                | coordinate = move delta model.coordinate
                , timeElapsed = model.timeElapsed + delta
              }
            , Cmd.none
            )


move : Float -> Coordinate -> Coordinate
move delta coordinate =
    let
        newCoordinate =
            coordinate |> Coordinate.addX (Px.px <| round (delta * 0.2))
    in
    if newCoordinate |> Coordinate.x |> Px.is (>) targetX then
        coordinate |> Coordinate.setX targetX

    else
        newCoordinate


reset : Model -> Model
reset model =
    if model.timeElapsed > 5000 then
        { model
            | coordinate = model.coordinate |> Coordinate.setX (Px.px 0)
            , timeElapsed = 0
        }

    else
        model



-- VIEW


view : Model -> Html Msg
view model =
    div [ HA.css [ Css.height (Css.px 200) ] ]
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
            , Css.left (model.coordinate |> Coordinate.x |> Px.toElmCss)
            , Css.top (model.coordinate |> Coordinate.y |> Px.toElmCss)
            , Shadow.style True
            ]
        ]
        []
