module Defered exposing (Defered(..), apply, andThen, defer, defer2, defer3, defer4, defer5, defer6, defer7, defer8, defer9, eval, map, wrap)


type Defered a
    = Defered (() -> a)


defer : (a -> b) -> a -> Defered b
defer f a =
    Defered (\_ -> f a)

eval : Defered a -> a
eval (Defered l) =
    l ()

defer2 : (a -> b -> c) -> a -> b -> Defered c
defer2 f a b =
    Defered (\_ -> f a b)


defer3 : (a -> b -> c -> d) -> a -> b -> c -> Defered d
defer3 f a b c =
    Defered (\_ -> f a b c)


defer4 : (a -> b -> c -> d -> e) -> a -> b -> c -> d -> Defered e
defer4 f a b c d =
    Defered (\_ -> f a b c d)


defer5 : (a -> b -> c -> d -> e -> f) -> a -> b -> c -> d -> e -> Defered f
defer5 f a b c d e =
    Defered (\_ -> f a b c d e)


defer6 : (a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> Defered g
defer6 f a b c d e g =
    Defered (\_ -> f a b c d e g)


defer7 : (a -> b -> c -> d -> e -> f -> g -> h) -> a -> b -> c -> d -> e -> f -> g -> Defered h
defer7 f a b c d e g h =
    Defered (\_ -> f a b c d e g h)


defer8 : (a -> b -> c -> d -> e -> f -> g -> h -> i) -> a -> b -> c -> d -> e -> f -> g -> h -> Defered i
defer8 f a b c d e g h i =
    Defered (\_ -> f a b c d e g h i)


defer9 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> j) -> a -> b -> c -> d -> e -> f -> g -> h -> i -> Defered j
defer9 f a b c d e g h i j =
    Defered (\_ -> f a b c d e g h i j)


map : (a -> b) -> Defered a -> Defered b
map f (Defered g) =
    Defered (g >> f)


andThen : (a -> Defered b) -> Defered a -> Defered b
andThen f (Defered a) =
    Defered (\_ -> eval (f (a ())))


wrap : a -> Defered a
wrap a =
    Defered (\_ -> a)

apply : a -> Defered (a -> b) -> Defered b
apply a (Defered f) = Defered (\_ -> f () a)