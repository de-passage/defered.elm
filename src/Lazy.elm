module Lazy exposing (Lazy(..), extract, lazy, lazy2, lazy3, lazy4, lazy5, lazy6, lazy7, lazy8, lazy9)


type Lazy a
    = Lazy (() -> a)


lazy : (a -> b) -> a -> Lazy b
lazy f a =
    Lazy (\_ -> f a)


lazy2 : (a -> b -> c) -> a -> b -> Lazy c
lazy2 f a b =
    Lazy (\_ -> f a b)


lazy3 : (a -> b -> c -> d) -> a -> b -> c -> Lazy d
lazy3 f a b c =
    Lazy (\_ -> f a b c)


lazy4 : (a -> b -> c -> d -> e) -> a -> b -> c -> d -> Lazy e
lazy4 f a b c d =
    Lazy (\_ -> f a b c d)


lazy5 : (a -> b -> c -> d -> e -> f) -> a -> b -> c -> d -> e -> Lazy f
lazy5 f a b c d e =
    Lazy (\_ -> f a b c d e)


lazy6 : (a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> Lazy g
lazy6 f a b c d e g =
    Lazy (\_ -> f a b c d e g)


lazy7 : (a -> b -> c -> d -> e -> f -> g -> h) -> a -> b -> c -> d -> e -> f -> g -> Lazy h
lazy7 f a b c d e g h =
    Lazy (\_ -> f a b c d e g h)


lazy8 : (a -> b -> c -> d -> e -> f -> g -> h -> i) -> a -> b -> c -> d -> e -> f -> g -> h -> Lazy i
lazy8 f a b c d e g h i =
    Lazy (\_ -> f a b c d e g h i)


lazy9 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> j) -> a -> b -> c -> d -> e -> f -> g -> h -> i -> Lazy j
lazy9 f a b c d e g h i j =
    Lazy (\_ -> f a b c d e g h i j)


extract : Lazy a -> a
extract (Lazy l) =
    l ()
