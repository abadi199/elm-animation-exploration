module Css.Animation exposing
    ( Px
    , Second
    , px
    , right
    , second
    , withAnimation
    )

import Html
import Html.Attributes as HA


type alias Html msg =
    List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg


type AnimatedHtml msg
    = Animated (List Animation) (Html msg)


type Animation
    = Right Px Second


type Px
    = Px Int


px : Int -> Px
px =
    Px


type Second
    = Second Float


second : Float -> Second
second =
    Second


right : Px -> Second -> Animation
right distance duration =
    Right distance duration


withAnimation : List Animation -> List (Html.Attribute msg) -> List (Html.Html msg) -> Html msg -> Html.Html msg
withAnimation animations attributes children html =
    let
        style =
            Html.node "style" [] [ Html.text keyframes ]
    in
    html
        (HA.style "animation"
            """moveRight 1s forwards
             , moveUp 1s 1s forwards"""
            :: attributes
        )
        (style :: children)


keyframes : String
keyframes =
    """
@keyframes moveRight {
    0% { transform: translate(0, 0); }
    100% { transform: translate(500px, 0); }
}

@keyframes moveUp {
    0% { transform: translate(500px, 0); }
    100% { transform: translate(500px, -500px); }
}

@keyframes blink {
    0% { opacity: 1; }
    100% { opacity: 0; }
}

@keyframes pulse {
    0% { transform: scale(1); }
    100% { transform: scale(1.2); }
}
    """
