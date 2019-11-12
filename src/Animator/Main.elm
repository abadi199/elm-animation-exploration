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
    { notification : Animator.State Bool
    , page : Animator.State Page
    }


type Page
    = BluePage
    | GreenPage
    | RedPage


type Msg
    = UserClickTestButton
    | TransitionComplete Bool
    | UserClickCloseNotificationButton
    | UserClickRedButton
    | UserClickBlueButton
    | UserClickGreenButton
    | PageTransitionComplete Page


init : () -> ( Model, Cmd Msg )
init _ =
    ( { notification = Animator.Idle False
      , page = Animator.Idle BluePage
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "Animator Demo"
    , body =
        [ Reset.meyerV2
        , Reset.borderBoxV201408
        , Animator.view
            { onFinish = PageTransitionComplete
            , render = viewPage
            , animationKind =
                \{ to } ->
                    case to of
                        BluePage ->
                            Animator.SlideInFromLeft

                        RedPage ->
                            Animator.SlideInFromTop

                        GreenPage ->
                            Animator.Fade
            }
            model.page
        , H.p
            [ HA.css
                [ displayFlex
                , position fixed
                , flexDirection column
                , top zero
                , width (px 500)
                , right (em 1)
                , margin2 zero auto
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
            |> List.map H.toUnstyled
    }


viewPage : Page -> H.Html Msg
viewPage page =
    H.div
        [ HA.css
            [ width (vw 100)
            , height (vh 100)
            , displayFlex
            , alignItems center
            , justifyContent center
            , flexDirection column
            , position relative
            , case page of
                BluePage ->
                    backgroundColor (rgba 130 177 217 1)

                GreenPage ->
                    backgroundColor (rgba 177 217 130 1)

                RedPage ->
                    backgroundColor (rgba 217 130 177 1)
            ]
        ]
        [ H.button
            [ HA.type_ "button"
            , HE.onClick UserClickTestButton
            , HA.css buttonCss
            ]
            [ H.text "Test" ]
        , H.div []
            [ H.button
                [ HA.css buttonCss
                , HE.onClick UserClickRedButton
                ]
                [ H.text "Red" ]
            , H.button
                [ HA.css buttonCss
                , HE.onClick UserClickGreenButton
                ]
                [ H.text "Green" ]
            , H.button
                [ HA.css buttonCss
                , HE.onClick UserClickBlueButton
                ]
                [ H.text "Blue" ]
            ]
        ]


buttonCss =
    [ padding2 (em 1) (em 2)
    , fontWeight bold
    , borderRadius (px 5)
    , margin (em 0.5)
    ]


animationKindForNotification : { from : Bool, to : Bool } -> Animator.AnimationKind
animationKindForNotification { from, to } =
    case ( from, to ) of
        ( False, True ) ->
            Animator.SlideInFromTop

        ( True, False ) ->
            Animator.Fade

        _ ->
            Animator.Fade


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

        UserClickRedButton ->
            ( { model | page = model.page |> Animator.transitionTo RedPage }, Cmd.none )

        UserClickBlueButton ->
            ( { model | page = model.page |> Animator.transitionTo BluePage }, Cmd.none )

        UserClickGreenButton ->
            ( { model | page = model.page |> Animator.transitionTo GreenPage }, Cmd.none )

        PageTransitionComplete page ->
            ( { model | page = Animator.Idle page }, Cmd.none )
