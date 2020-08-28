module Drag exposing (Drag, distance, from, getCurrent, getDimensions, getStart, to)

import Point exposing (Point)


type Drag
    = Drag
        { start : Point
        , current : Point
        }


getStart : Drag -> Point
getStart (Drag { start }) =
    start


getCurrent : Drag -> Point
getCurrent (Drag { current }) =
    current


getDimensions : Drag -> { x : Int, y : Int, w : Int, h : Int }
getDimensions (Drag { start, current }) =
    { x = min start.x current.x
    , y = min start.y current.y
    , w = abs (start.x - current.x)
    , h = abs (start.y - current.y)
    }


from : Point -> Drag
from p =
    Drag { start = p, current = p }


distance : Drag -> Point
distance (Drag { start, current }) =
    Point.sub current start


to : Point -> Drag -> Drag
to point (Drag { start }) =
    Drag { start = start, current = point }
