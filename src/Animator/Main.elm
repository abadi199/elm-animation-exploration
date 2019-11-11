module Animator.Main exposing (main)

import Animator.Animator as Animator
import Browser
import Css exposing (..)
import Css.Reset as Reset
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as HA
import Html.Styled.Events as HE


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { notification : Animator.State Bool }


type Msg
    = NoOp
    | UserClickTestButton
    | TransitionComplete Bool
    | UserClickCloseNotificationButton


type Page
    = One OneData
    | Two TwoData


type alias OneData =
    {}


type alias TwoData =
    {}


init : () -> ( Model, Cmd Msg )
init _ =
    ( { notification = Animator.Idle False }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "Animator Demo"
    , body =
        [ Reset.meyerV2
        , Reset.borderBoxV201408
        , H.div
            [ HA.css
                [ width (vw 100)
                , height (vh 100)
                , displayFlex
                , alignItems center
                , justifyContent center
                , flexDirection column
                , position relative
                ]
            ]
            [ H.button
                [ HA.type_ "button"
                , HE.onClick UserClickTestButton
                , HA.css
                    [ padding2 (em 1) (em 2)
                    , fontWeight bold
                    , borderRadius (px 5)
                    ]
                ]
                [ H.text "Test" ]
            , H.p
                [ HA.css
                    [ displayFlex
                    , position absolute
                    , flexDirection column
                    , top zero
                    , width (px 500)
                    ]
                ]
                [ Animator.view
                    { onFinish = TransitionComplete
                    , render = viewNotification
                    , animationKind = animationKindForNotification
                    }
                    model.notification
                ]
            ]
        ]
            |> List.map H.toUnstyled
    }


animationKindForNotification : { from : Bool, to : Bool } -> Animator.AnimationKind
animationKindForNotification { from, to } =
    case ( from, to ) of
        ( False, True ) ->
            Animator.Fade

        ( True, False ) ->
            Animator.Fade

        _ ->
            Animator.NoAnimation


viewNotification : Bool -> H.Html Msg
viewNotification flag =
    if flag then
        H.div
            [ HA.css
                [ backgroundColor (rgba 252 129 129 1)
                , fontWeight bold
                , fontFamily sansSerif
                , padding (em 1)
                , borderRadius (px 5)
                , margin4 (em 2) (em 1) (em 1) (em 1)
                , property "box-shadow"
                    "2px 2px 5px rgba(0,0,0,0.25)"
                , boxSizing contentBox
                , displayFlex
                , flexDirection row
                , justifyContent spaceBetween
                , alignItems center
                ]
            ]
            [ H.span [] [ H.text "Warning - This is a test!!!" ]
            , H.button
                [ HA.type_ "button"
                , HA.css
                    [ border zero
                    , backgroundColor (rgba 0 0 0 0.25)
                    , fontWeight bold
                    , padding2 (px 5) (px 10)
                    ]
                , HE.onClick UserClickCloseNotificationButton
                ]
                [ H.text "CLOSE" ]
            ]

    else
        H.text ""


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UserClickTestButton ->
            ( { model | notification = Animator.Transitioning False True }
            , Cmd.none
            )

        TransitionComplete page ->
            ( { model | notification = Animator.Idle page }
            , Cmd.none
            )

        UserClickCloseNotificationButton ->
            ( { model | notification = Animator.Transitioning True False }, Cmd.none )
