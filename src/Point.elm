module Point exposing (Point, sub, add)

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