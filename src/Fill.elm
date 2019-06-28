module Fill exposing
    ( Fill
    , backwards
    , default
    , encode
    , forwards
    , none
    )

import Json.Encode as JE


default : Fill
default =
    None


type Fill
    = None
    | Forwards
    | Backwards


none : Fill
none =
    None


forwards : Fill
forwards =
    Forwards


backwards : Fill
backwards =
    Backwards


encode : Fill -> JE.Value
encode fill =
    case fill of
        None ->
            JE.string "none"

        Forwards ->
            JE.string "forwards"

        Backwards ->
            JE.string "backwards"
