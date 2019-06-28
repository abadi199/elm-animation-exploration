module Js.AnimationTest exposing (suite)

import Coordinate exposing (coordinate)
import Count
import Degree exposing (deg)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Js.Animation as Animation
import Json.Encode as JE
import Px exposing (px)
import Second exposing (second)
import Test exposing (..)


jsonToString =
    Animation.jsonToString


suite : Test
suite =
    describe "Js.Animation module"
        [ animationsToString, encodeKeyframe ]


animationsToString : Test
animationsToString =
    describe "Js.Animation.animationsToString"
        [ test "rotation" <|
            \_ ->
                Animation.animationsToString
                    [ Animation.rotate (deg 360) (second 2)
                        |> Animation.withCount Count.infinite
                    ]
                    |> Expect.equal
                        """{"keyframes":[{"transform":"rotate(360deg)"}],"options":{"duration":2000,"iterations":"Infinity"}}"""
        ]


encodeKeyframe : Test
encodeKeyframe =
    describe "Js.Animation."
        [ test "translate" <|
            \_ ->
                Animation.translate (coordinate { x = px 100, y = px 200 }) (second 2)
                    |> Animation.encodeKeyframe
                    |> Animation.jsonToString
                    |> Expect.equal
                        (JE.object [ ( "transform", JE.string "translate(100px,200px)" ) ] |> jsonToString)
        , test "rotation" <|
            \_ ->
                Animation.rotate (deg 360) (second 2)
                    |> Animation.encodeKeyframe
                    |> jsonToString
                    |> Expect.equal
                        (JE.object [ ( "transform", JE.string "rotate(360deg)" ) ] |> jsonToString)
        , test "delayed" <|
            \_ ->
                Animation.rotate (deg 360) (second 2)
                    |> Animation.delay (second 10)
                    |> Animation.encodeKeyframe
                    |> jsonToString
                    |> Expect.equal
                        (JE.object [ ( "transform", JE.string "rotate(360deg)" ) ] |> jsonToString)
        ]
