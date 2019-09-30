module Point exposing (Point, sub, add, mulPointWise, dist, taxiDist, toIntString, randomPoint, randomPoints, xToPixel, yToPixel)

import Random

type alias Point =
  { x: Float
  , y: Float
  }

toIntString : Point -> String
toIntString p =
  (String.fromInt <| floor p.x) ++ " " ++ (String.fromInt <| floor p.y)

toString : Point -> String
toString p =
  (String.fromFloat p.x) ++ " " ++ (String.fromFloat p.y)

xToPixel : Point -> String
xToPixel p =
  (String.fromInt <| floor p.x) ++ "px"

yToPixel : Point -> String
yToPixel p =
  (String.fromInt <| floor p.y) ++ "px"

sub : Point -> Point -> Point
sub a b =
  Point (a.x - b.x) (a.y - b.y)

add : Point -> Point -> Point
add a b =
  Point (a.x + b.x) (a.y + b.y)

mulPointWise : Point -> Point -> Point
mulPointWise a b =
  Point (a.x * b.x) (a.y * b.y)

dist : Point -> Point -> Float
dist a b =
  sqrt <| (b.x - a.x)^2 + (b.y - a.y)^2

taxiDist : Point -> Point -> Float
taxiDist a b =
  abs (b.x - a.x) + abs (b.y - a.y)


randomPoint : Float -> Float -> Float -> Float -> Random.Generator Point
randomPoint xmin xmax ymin ymax =
  Random.map2 Point
    (Random.float xmin xmax)
    (Random.float ymin ymax)

randomPoints : Int -> Float -> Float -> Float -> Float -> Random.Generator (List Point)
randomPoints n xmin xmax ymin ymax =
  Random.list n <| randomPoint xmin xmax ymin ymax
