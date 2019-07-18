module Caterpillar.FastCaterpillar exposing (view)

import Array exposing (Array)
import Caterpillar.Shadow as Shadow
import Count
import Css exposing (..)
import Dimension exposing (Dimension, dimension)
import Easing
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as HA
import Js.Animation as Animation exposing (Keyframe)
import Js.Animation.Options as Options
import Millisecond exposing (Millisecond, millisecond)
import Px
import PxPerMs exposing (PxPerMs)
import Time exposing (Posix)


caterpillarDimension : Dimension
caterpillarDimension =
    let
        width =
            1000

        ratio =
            452
                / 1000
    in
    dimension { width = Px.px width, height = Px.px (Basics.round <| width * ratio) }


type alias Options =
    { imageUrl : String
    , windowDimension : Dimension
    , showShadow : Bool
    , loopDuration : Millisecond
    }


view : Options -> Html msg
view { imageUrl, windowDimension, showShadow, loopDuration } =
    Animation.styledNode
        [ Animation.backgroundPosition "0 0"
        , Animation.backgroundPosition "0 -1000%"
        ]
        (Options.default { duration = loopDuration }
            |> Options.withEasing (Easing.steps 10)
            |> Options.withIterations Count.infinite
        )
        []
        (H.div
            [ HA.css
                [ position absolute
                , backgroundImage (url imageUrl)
                , backgroundSize (pct 100)
                , left
                    (windowDimension
                        |> Dimension.width
                        |> Px.divideBy 2
                        |> Px.add (Px.px ((caterpillarDimension |> Dimension.width |> Px.toInt |> toFloat |> negate |> Basics.round) // 2))
                        |> Px.toElmCss
                    )
                , bottom (px 200)
                , width (caterpillarDimension |> Dimension.width |> Px.toElmCss)
                , height (caterpillarDimension |> Dimension.height |> Px.toElmCss)
                , Shadow.style showShadow
                ]
            ]
            []
        )
