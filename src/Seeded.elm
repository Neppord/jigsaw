module Seeded exposing (Seeded(..), unwrap, map)

import Random


type Seeded a
    = Seeded Random.Seed a


map : (a -> b) -> Seeded a -> Seeded b
map f (Seeded seed a) =
    Seeded seed (f a)

unwrap: Seeded a  -> a
unwrap (Seeded _ a) = a
