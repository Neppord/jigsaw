module At exposing (At(..), at, map, moveBy, moveTo, position)


type At a
    = At ( Int, Int ) a


at : ( Int, Int ) -> a -> At a
at =
    At


map : (a -> b) -> At a -> At b
map f (At p a) =
    At p (f a)


moveTo : ( Int, Int ) -> At a -> At a
moveTo p (At _ a) =
    At p a


moveBy : ( Int, Int ) -> At a -> At a
moveBy ( dx, dy ) (At ( x, y ) a) =
    At ( x + dx, y + dy ) a


position : At a -> ( Int, Int )
position (At p _) =
    p
