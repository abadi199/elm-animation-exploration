module Animator.Animator exposing (AnimationKind(..), State(..), view)

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
                    ]
                ]
                [ render p
                ]

        Transitioning from to ->
            let
                animationKindValue =
                    { from = from, to = to }
                        |> animationKind
            in
            H.node "elm-animator"
                [ HA.attribute "transitioning" "true"
                , HA.attribute "kind" (animationKindToString animationKindValue)
                , HE.on "finish" (JD.succeed (onFinish to))
                , HA.css
                    [ overflow hidden
                    ]
                ]
                [ H.node "elm-animator-to"
                    [ HA.css
                        [ width (pct 100)
                        , case animationKindValue of
                            SlideInFromTop ->
                                display none

                            _ ->
                                display inlineBlock
                        , case animationKindValue of
                            Fade ->
                                opacity (num 0)

                            _ ->
                                opacity (num 1)
                        ]
                    ]
                    [ render to ]
                , H.node "elm-animator-from"
                    [ HA.css
                        [ width (pct 100)
                        , case animationKindValue of
                            SlideInFromTop ->
                                display none

                            _ ->
                                display inlineBlock
                        , case animationKindValue of
                            Fade ->
                                opacity (num 1)

                            _ ->
                                opacity (num 0)
                        ]
                    ]
                    [ render from ]
                ]
