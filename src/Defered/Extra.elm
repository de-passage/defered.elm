module Defered.Extra exposing
    ( andApply
    , before
    , deferedLift
    , deferedLift2
    , deferedLift3
    , deferedLift4
    , deferedLift5
    , deferedLift6
    , deferedLift7
    , deferedLift8
    , deferedLift9
    , filterList
    , lift
    , lift2
    , lift3
    , lift4
    , lift5
    , lift6
    , lift7
    , lift8
    , lift9
    , mapList
    , maybeWithDefault
    , sequence
    )

import Defered exposing (Defered(..), defer, defer2, eval, wrap)


andApply : Defered (a -> b) -> Defered a -> Defered b
andApply (Defered f) (Defered a) =
    Defered (\_ -> f () (a ()))


before : (c -> a) -> Defered (a -> b) -> Defered (c -> b)
before f (Defered g) =
    Defered (\_ c -> g () (f c))


mapList : (a -> Defered b) -> List a -> Defered (List b)
mapList f =
    defer2 List.map (\a -> eval (f a))


sequence : List (Defered a) -> Defered (List a)
sequence =
    defer2 List.map eval


filterList : (a -> Defered Bool) -> List a -> Defered (List a)
filterList f =
    defer2 List.filter (\a -> eval (f a))


maybeWithDefault : Maybe a -> Defered a -> a
maybeWithDefault m (Defered a) =
    case m of
        Nothing ->
            a ()

        Just v ->
            v


lift : (a -> b) -> Defered a -> b
lift f (Defered a) =
    f (a ())


lift2 : (a -> b -> c) -> Defered a -> Defered b -> c
lift2 f (Defered a) (Defered b) =
    f (a ()) (b ())


lift3 : (a -> b -> c -> d) -> Defered a -> Defered b -> Defered c -> d
lift3 f (Defered a) (Defered b) (Defered c) =
    f (a ()) (b ()) (c ())


lift4 : (a -> b -> c -> d -> e) -> Defered a -> Defered b -> Defered c -> Defered d -> e
lift4 f (Defered a) (Defered b) (Defered c) (Defered d) =
    f (a ()) (b ()) (c ()) (d ())


lift5 : (a -> b -> c -> d -> e -> f) -> Defered a -> Defered b -> Defered c -> Defered d -> Defered e -> f
lift5 f (Defered a) (Defered b) (Defered c) (Defered d) (Defered e) =
    f (a ()) (b ()) (c ()) (d ()) (e ())


lift6 : (a -> b -> c -> d -> e -> f -> g) -> Defered a -> Defered b -> Defered c -> Defered d -> Defered e -> Defered f -> g
lift6 f (Defered a) (Defered b) (Defered c) (Defered d) (Defered e) (Defered g) =
    f (a ()) (b ()) (c ()) (d ()) (e ()) (g ())


lift7 : (a -> b -> c -> d -> e -> f -> g -> h) -> Defered a -> Defered b -> Defered c -> Defered d -> Defered e -> Defered f -> Defered g -> h
lift7 f (Defered a) (Defered b) (Defered c) (Defered d) (Defered e) (Defered g) (Defered h) =
    f (a ()) (b ()) (c ()) (d ()) (e ()) (g ()) (h ())


lift8 : (a -> b -> c -> d -> e -> f -> g -> h -> i) -> Defered a -> Defered b -> Defered c -> Defered d -> Defered e -> Defered f -> Defered g -> Defered h -> i
lift8 f (Defered a) (Defered b) (Defered c) (Defered d) (Defered e) (Defered g) (Defered h) (Defered i) =
    f (a ()) (b ()) (c ()) (d ()) (e ()) (g ()) (h ()) (i ())


lift9 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> j) -> Defered a -> Defered b -> Defered c -> Defered d -> Defered e -> Defered f -> Defered g -> Defered h -> Defered i -> j
lift9 f (Defered a) (Defered b) (Defered c) (Defered d) (Defered e) (Defered g) (Defered h) (Defered i) (Defered j) =
    f (a ()) (b ()) (c ()) (d ()) (e ()) (g ()) (h ()) (i ()) (j ())


deferedLift : (a -> b) -> Defered a -> Defered b
deferedLift f (Defered a) =
    Defered (\_ -> f (a ()))


deferedLift2 : (a -> b -> c) -> Defered a -> Defered b -> Defered c
deferedLift2 f (Defered a) (Defered b) =
    Defered (\_ -> f (a ()) (b ()))


deferedLift3 : (a -> b -> c -> d) -> Defered a -> Defered b -> Defered c -> Defered d
deferedLift3 f (Defered a) (Defered b) (Defered c) =
    Defered (\_ -> f (a ()) (b ()) (c ()))


deferedLift4 : (a -> b -> c -> d -> e) -> Defered a -> Defered b -> Defered c -> Defered d -> Defered e
deferedLift4 f (Defered a) (Defered b) (Defered c) (Defered d) =
    Defered (\_ -> f (a ()) (b ()) (c ()) (d ()))


deferedLift5 : (a -> b -> c -> d -> e -> f) -> Defered a -> Defered b -> Defered c -> Defered d -> Defered e -> Defered f
deferedLift5 f (Defered a) (Defered b) (Defered c) (Defered d) (Defered e) =
    Defered (\_ -> f (a ()) (b ()) (c ()) (d ()) (e ()))


deferedLift6 : (a -> b -> c -> d -> e -> f -> g) -> Defered a -> Defered b -> Defered c -> Defered d -> Defered e -> Defered f -> Defered g
deferedLift6 f (Defered a) (Defered b) (Defered c) (Defered d) (Defered e) (Defered g) =
    Defered (\_ -> f (a ()) (b ()) (c ()) (d ()) (e ()) (g ()))


deferedLift7 : (a -> b -> c -> d -> e -> f -> g -> h) -> Defered a -> Defered b -> Defered c -> Defered d -> Defered e -> Defered f -> Defered g -> Defered h
deferedLift7 f (Defered a) (Defered b) (Defered c) (Defered d) (Defered e) (Defered g) (Defered h) =
    Defered (\_ -> f (a ()) (b ()) (c ()) (d ()) (e ()) (g ()) (h ()))


deferedLift8 : (a -> b -> c -> d -> e -> f -> g -> h -> i) -> Defered a -> Defered b -> Defered c -> Defered d -> Defered e -> Defered f -> Defered g -> Defered h -> Defered i
deferedLift8 f (Defered a) (Defered b) (Defered c) (Defered d) (Defered e) (Defered g) (Defered h) (Defered i) =
    Defered (\_ -> f (a ()) (b ()) (c ()) (d ()) (e ()) (g ()) (h ()) (i ()))


deferedLift9 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> j) -> Defered a -> Defered b -> Defered c -> Defered d -> Defered e -> Defered f -> Defered g -> Defered h -> Defered i -> Defered j
deferedLift9 f (Defered a) (Defered b) (Defered c) (Defered d) (Defered e) (Defered g) (Defered h) (Defered i) (Defered j) =
    Defered (\_ -> f (a ()) (b ()) (c ()) (d ()) (e ()) (g ()) (h ()) (i ()) (j ()))
