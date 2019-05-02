module Millisecond exposing (Millisecond, add, isAfter, millisecond, toString)


type Millisecond
    = Millisecond Float


millisecond =
    Millisecond


isAfter : Millisecond -> Millisecond -> Bool
isAfter (Millisecond from) (Millisecond to) =
    from > to


add : Millisecond -> Millisecond -> Millisecond
add (Millisecond a) (Millisecond b) =
    Millisecond (a + b)


toString : Millisecond -> String
toString (Millisecond ms) =
    String.fromFloat ms ++ "ms"
