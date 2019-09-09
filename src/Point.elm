module Point exposing (Point, sub, add, randomPoint, randomPoints)

import Random

type alias Point =
  { x: Int
  , y: Int
  }

sub : Point -> Point -> Point
sub a b =
  Point (a.x - b.x) (a.y - b.y)

add : Point -> Point -> Point
add a b =
  Point (a.x + b.x) (a.y + b.y)


randomPoints : Int -> Int -> Int -> Random.Generator (List Point)
randomPoints n min max =
  Random.list n <| randomPoint min max

randomPoint : Int -> Int -> Random.Generator Point
randomPoint min max =
  Random.map2
    (\x y -> Point x y)
    (Random.int min max)
    (Random.int min max)