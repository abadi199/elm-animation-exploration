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
import Speed exposing (Speed)
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
    let
        keyframes =
            case state of
                Expanding ->
                    [ Animation.backgroundPosition "0 0"
                    , Animation.backgroundPosition "0 -600%"
                    ]

                Contracting ->
                    [ Animation.backgroundPosition "0 -600%"
                    , Animation.backgroundPosition "0 -1000%"
                    ]

        easing =
            case state of
                Expanding ->
                    Easing.steps 6

                Contracting ->
                    Easing.steps 4

        onFinish =
            case state of
                Expanding ->
                    HE.on "finish" <| JD.succeed onFinishExpanding

                Contracting ->
                    HE.on "finish" <| JD.succeed onFinishContracting

        scaleFactor =
            (windowDimension |> Dimension.width |> Px.toFloat) / 1920

        caterpillarScaledDimension =
            caterpillarDimension
                |> Dimension.scale scaleFactor
    in
    Animation.styledNode
        keyframes
        (Options.default { duration = loopDuration }
            |> Options.withEasing easing
            |> Options.withFill Fill.forwards
        )
        [ onFinish ]
        (H.div
            [ HA.css
                [ position absolute
                , backgroundImage (url imageUrl)
                , backgroundSize (pct 100)
                , left
                    (windowDimension
                        |> Dimension.width
                        |> Px.divideBy 2
                        |> Px.add
                            (Px.px
                                ((caterpillarScaledDimension
                                    |> Dimension.width
                                    |> Px.toInt
                                    |> toFloat
                                    |> negate
                                    |> Basics.round
                                 )
                                    // 2
                                )
                            )
                        |> Px.toElmCss
                    )
                , bottom (pct 25)
                , width (caterpillarScaledDimension |> Dimension.width |> Px.toElmCss)
                , height (caterpillarScaledDimension |> Dimension.height |> Px.toElmCss)
                , Shadow.style showShadow
                ]
            ]
            []
        )
