module Caterpillar.Caterpillar exposing (view)

import Array exposing (Array)
import Caterpillar.Shadow as Shadow
import Css exposing (..)
import Dimension exposing (Dimension, dimension)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as HA
import Millisecond exposing (Millisecond, millisecond)
import Px
import Time exposing (Posix)


caterpillarDimension : Dimension
caterpillarDimension =
    let
        width =
            600

        ratio =
            452 / 1000
    in
    dimension { width = Px.px width, height = Px.px (Basics.round <| width * ratio) }


backgroundPositions : Array Style
backgroundPositions =
    Array.fromList
        [ backgroundPosition2 zero zero
        , backgroundPosition2 (pct -100) zero
        , backgroundPosition2 (pct -200) zero
        , backgroundPosition2 zero (pct -100)
        , backgroundPosition2 (pct -100) (pct -100)
        , backgroundPosition2 (pct -200) (pct -100)
        , backgroundPosition2 zero (pct -200)
        , backgroundPosition2 (pct -100) (pct -200)
        , backgroundPosition2 (pct -200) (pct -200)
        , backgroundPosition2 zero (pct -300)
        ]


calculateBackgroundPositionIndex : Posix -> Int
calculateBackgroundPositionIndex time =
    let
        loopDuration =
            2000

        numberOfFrames =
            Array.length backgroundPositions

        mod =
            time |> Time.posixToMillis |> modBy loopDuration
    in
    mod * numberOfFrames // loopDuration


view : { caterpillar : String, windowDimension : Dimension, showShadow : Bool, time : Posix } -> Html msg
view { caterpillar, windowDimension, showShadow, time } =
    let
        positionIndex =
            calculateBackgroundPositionIndex time
    in
    H.div
        [ HA.css
            [ position absolute
            , backgroundImage (url caterpillar)
            , backgroundSize (pct 300)
            , backgroundPositions |> Array.get positionIndex |> Maybe.withDefault (backgroundPosition2 zero zero)
            , left
                (windowDimension
                    |> Dimension.width
                    |> Px.divideBy 2
                    |> Px.add (Px.px (Basics.round (caterpillarDimension |> Dimension.width |> Px.toInt |> toFloat |> negate) // 2))
                    |> Px.toElmCss
                )
            , bottom (px 200)
            , width (caterpillarDimension |> Dimension.width |> Px.toElmCss)
            , height (caterpillarDimension |> Dimension.height |> Px.toElmCss)
            , Shadow.style showShadow
            ]
        ]
        []
