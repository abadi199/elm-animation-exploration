module Animator.Animator exposing
    ( AnimationKind(..)
    , State(..)
    , transitionTo
    , view
    )

import Css exposing (..)
import Html.Styled as H
import Html.Styled.Attributes as HA
import Html.Styled.Events as HE
import Json.Decode as JD


type State page
    = Idle page
    | Transitioning page page


type AnimationKind
    = SlideInFromTop
    | Fade
    | NoAnimation


animationKindToString : AnimationKind -> String
animationKindToString kind =
    case kind of
        SlideInFromTop ->
            "SlideInFromTop"

        Fade ->
            "Fade"

        NoAnimation ->
            "NoAnimation"


view :
    { onFinish : page -> msg
    , render : page -> H.Html msg
    , animationKind : { from : page, to : page } -> AnimationKind
    }
    -> State page
    -> H.Html msg
view { onFinish, render, animationKind } state =
    case state of
        Idle p ->
            H.node "elm-animator"
                [ HA.css
                    [ overflow hidden
                    , height (vh 100)
                    , display inlineBlock
                    ]
                ]
                [ render p
                ]

        Transitioning from to ->
            let
                animationKindValue =
                    { from = from, to = to }
                        |> animationKind

                baseCss =
                    [ width (pct 100) ]
            in
            H.node "elm-animator"
                [ HA.attribute "transitioning" "true"
                , HA.attribute "kind" (animationKindToString animationKindValue)
                , HE.on "finish" (JD.succeed (onFinish to))
                , case animationKindValue of
                    Fade ->
                        HA.css
                            [ property "display" "inline-grid"
                            , property "grid-template-areas" "\"main\""
                            ]

                    _ ->
                        HA.css
                            [ overflow hidden
                            , display inlineBlock
                            , height (vh 100)
                            ]
                ]
                [ H.node "elm-animator-to"
                    [ case animationKindValue of
                        SlideInFromTop ->
                            HA.css
                                (display inlineBlock
                                    :: transform (translateY (pct -100))
                                    :: baseCss
                                )

                        Fade ->
                            HA.css
                                (opacity (num 0)
                                    :: property "grid-area" "main"
                                    :: baseCss
                                )

                        NoAnimation ->
                            HA.css baseCss
                    ]
                    [ render to ]
                , H.node "elm-animator-from"
                    [ case animationKindValue of
                        SlideInFromTop ->
                            HA.css
                                (display inlineBlock
                                    :: transform (translateY (pct -100))
                                    :: baseCss
                                )

                        Fade ->
                            HA.css
                                (display inlineBlock
                                    :: opacity (num 1)
                                    :: property "grid-area" "main"
                                    :: baseCss
                                )

                        NoAnimation ->
                            HA.css baseCss
                    ]
                    [ render from ]
                ]


transitionTo : page -> State page -> State page
transitionTo to state =
    case state of
        Idle from ->
            Transitioning from to

        Transitioning from _ ->
            Transitioning from to
