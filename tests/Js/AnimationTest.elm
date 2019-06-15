module Js.AnimationTest exposing (suite)

import Count
import Degree exposing (deg)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Js.Animation as Animation
import Json.Encode as JE
import Px exposing (px)
import Second exposing (second)
import Test exposing (..)


suite : Test
suite =
    describe "Js.Animation module"
        [ animationsToString, encode ]


animationsToString : Test
animationsToString =
    describe "Animation.animationsToString"
        [ test "rotation" <|
            \_ ->
                Animation.animationsToString
                    [ Animation.rotate (deg 360) (second 2)
                        |> Animation.withCount Count.infinite
                    ]
                    |> Expect.equal
                        """{"keyframes":[{"transform":"rotate(0deg)"},{"transform":"rotate(360deg)"}],"options":{"duration":3000,"iterations":"Infinity"}}"""
        ]


encode : Test
encode =
    let
        jsonToString =
            JE.encode 2
    in
    describe "Animation.encode"
        [ test "rotation" <|
            \_ ->
                Animation.encode
                    (Animation.rotate (deg 360) (second 2)
                        |> Animation.withCount Count.infinite
                    )
                    |> jsonToString
                    |> Expect.equal
                        (JE.object [] |> jsonToString)
        ]
