module Js.Animation.Events exposing (onFinish, onFinishUnstyled)

import Html as H
import Html.Events as HE
import Html.Styled as HS
import Html.Styled.Events as HSE
import Json.Decode as JD


onFinish : msg -> HS.Attribute msg
onFinish msg =
    HSE.on "finish" <| JD.succeed msg


onFinishUnstyled : msg -> H.Attribute msg
onFinishUnstyled msg =
    HE.on "finish" <| JD.succeed msg
