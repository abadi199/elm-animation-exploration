module Caterpillar.FastCaterpillar exposing
    ( State
    , contracting
    , expanding
    , initialState
    , view
    )

import Array exposing (Array)
import Caterpillar.Shadow as Shadow
import Count
import Css exposing (..)
import Dimension exposing (Dimension, dimension)
import Easing
import Fill
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as HA
import Html.Styled.Events as HE
import Js.Animation as Animation exposing (Keyframe)
import Js.Animation.Options as Options
import Json.Decode as JD
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


type State
    = Expanding
    | Contracting


initialState : State
initialState =
    Expanding


expanding : State
expanding =
    Expanding


contracting : State
contracting =
    Contracting


type alias Options msg =
    { imageUrl : String
    , windowDimension : Dimension
    , showShadow : Bool
    , loopDuration : Millisecond
    , onFinishExpanding : msg
    , onFinishContracting : msg
    }


view : Options msg -> State -> Html msg
view { imageUrl, windowDimension, showShadow, loopDuration, onFinishExpanding, onFinishContracting } state =
    Animation.styledNode
        (case state of
            Expanding ->
                [ Animation.backgroundPosition "0 0"
                , Animation.backgroundPosition "0 -500%"
                ]

            Contracting ->
                [ Animation.backgroundPosition "0 -500"
                , Animation.backgroundPosition "0 -1000%"
                ]
        )
        (Options.default { duration = loopDuration }
            |> Options.withEasing (Easing.steps 5)
            |> Options.withFill Fill.forwards
        )
        [ case state of
            Expanding ->
                HE.on "finish" <| JD.succeed onFinishExpanding

            Contracting ->
                HE.on "finish" <| JD.succeed onFinishContracting
        ]
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
