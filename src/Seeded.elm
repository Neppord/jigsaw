module Seeded exposing (Seeded(..), step, embed, unwrap, map)

import Random


type Seeded a
    = Seeded Random.Seed a


map : (a -> b) -> Seeded a -> Seeded b
map f (Seeded seed a) =
    Seeded seed (f a)

unwrap: Seeded a  -> a
unwrap (Seeded _ a) = a

embed : Seeded (a, b) -> (Seeded a, b)
embed (Seeded seed (a, b)) = (Seeded seed a, b)

step : Seeded (Random.Generator a) -> (Seeded a)
step (Seeded seed gen) = 
    let
        (a, nextSeed) = 
            Random.step gen seed
    in
     Seeded nextSeed a
