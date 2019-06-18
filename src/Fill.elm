module Fill exposing
    ( Fill
    , backwards
    , default
    , forwards
    , none
    )


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
