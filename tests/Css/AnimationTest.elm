module Css.AnimationTest exposing (suite)

import Css.Animation as Animation
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Px exposing (px)
import Second exposing (second)
import Test exposing (..)


suite : Test
suite =
    describe "Css.Animation module"
        [ toKeyframeSuite ]


toKeyframeSuite : Test
toKeyframeSuite =
    describe "Animation.toKeyframe function"
        [ test "process sequence" <|
            \_ ->
                Animation.toKeyframe "test"
                    (Animation.sequence
                        [ Animation.translate { x = px 100, y = px 0 } (second 2)
                        , Animation.translate { x = px 0, y = px 200 } (second 1)
                        , Animation.translate { x = px 50, y = px 50 } (second 3)
                        ]
                    )
                    |> Expect.equal
                        ("@keyframes test { "
                            ++ "33% { transform: translate(100px,0px); } "
                            ++ "50% { transform: translate(100px,200px); } "
                            ++ "100% { transform: translate(150px,250px); }"
                            ++ " }"
                        )
        ]
