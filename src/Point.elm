module Point exposing
    ( Point
    , add
    , dist
    , dot
    , fromPair
    , randomPoint
    , randomPoints
    , sub
    , taxiDist
    , toPair
    , toString
    )

import Random


type alias Point =
    { x : Int
    , y : Int
    }


toString : Point -> String
toString p =
    String.fromInt p.x ++ " " ++ String.fromInt p.y


sub : Point -> Point -> Point
sub a b =
    Point (a.x - b.x) (a.y - b.y)


add : Point -> Point -> Point
add a b =
    Point (a.x + b.x) (a.y + b.y)


dot : Point -> Point -> Point
dot a b =
    Point (a.x * b.x) (a.y * b.y)


dist : Point -> Point -> Float
dist a b =
    sqrt <| (toFloat b.x - toFloat a.x) ^ 2 + (toFloat b.y - toFloat a.y) ^ 2


taxiDist : Point -> Point -> Int
taxiDist a b =
    abs (b.x - a.x) + abs (b.y - a.y)


randomPoint : Int -> Int -> Int -> Int -> Random.Generator Point
randomPoint xmin xmax ymin ymax =
    Random.map2 Point
        (Random.int xmin xmax)
        (Random.int ymin ymax)


randomPoints : Int -> Int -> Int -> Int -> Int -> Random.Generator (List Point)
randomPoints n xmin xmax ymin ymax =
    Random.list n <| randomPoint xmin xmax ymin ymax


toPair : Point -> ( Int, Int )
toPair point =
    ( point.x, point.y )


fromPair : ( Int, Int ) -> Point
fromPair ( x, y ) =
    Point x y
