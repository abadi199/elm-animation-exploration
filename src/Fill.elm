module Fill exposing
    ( Fill
    , backwards
    , forwards
    , none
    )


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
