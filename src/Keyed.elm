module Keyed exposing (Keyed, map)

import Browser.Navigation


type alias Keyed a =
    { key : Browser.Navigation.Key
    , value : a
    }


map : (a -> b) -> Keyed a -> Keyed b
map f { key, value } =
    { key = key, value = f value }
