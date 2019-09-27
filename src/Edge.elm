module Edge exposing (..)

import Random
import Array
import Dict as D

import Point exposing (Point)
import Util

type alias Model =
  { curves : String
  , seed : Random.Seed
  , nx : Int
  , ny : Int
  }

makeEdgePoints : Int -> Random.Seed -> (Array.Array EdgePoints, Random.Seed)
makeEdgePoints n seed =
  let
    -- We only generate 8 points because the first and last will always be the same!
    (offsets, seed1) =
      Random.step (Random.list n <| Point.randomPoints 8 -5 5 -5 5) seed

    (chiralities, seed2) =
      Random.step (Random.list n <| Random.int 0 1) seed1

    -- Chirality 0 means the 'ear' is pointing up, 1 means it points down
    setChirality : EdgePoints -> Int -> List Point
    setChirality ep ch =
      if ch == 0 then
        ep
      else
        List.map (\p -> Point p.x -p.y) ep

    -- Again, because the first and last points are always the same
    -- (0 0 and 200 0, respectively), we 'pad' the generated points with 0 0 on both ends
    translatePoints ep =
      List.map2 Point.add defaultPoints ([Point 0 0] ++ ep ++ [Point 0 0])

    edgePoints =
      List.map2 (setChirality << translatePoints) offsets chiralities
        |> Array.fromList
  in
    (edgePoints, seed2)


type Edge
  = Curved { start : Point, b1 : Bezier, b2 : Bezier, b3 : Bezier, b4 : Bezier}
  | Flat { start : Point, end : Point }

type alias EdgePoints = List Point

type Bezier
  = C Point Point Point
  | S Point Point


defaultCurvedEdge =
  Curved
    { start = Point 0 0
    , b1 = C (Point 50 20) (Point 100 25) (Point 80 0)
    , b2 = S (Point 70 -40) (Point 100 -40)
    , b3 = S (Point 140 -25) (Point 120 0)
    , b4 = S (Point 150 20) (Point 200 0)
    }

defaultPoints =
  [ Point 0 0
  , Point 50 20
  , Point 100 25
  , Point 80 0
  , Point 70 -40
  , Point 100 -40
  , Point 140 -25
  , Point 120 0
  , Point 150 20
  , Point 200 0
  ]


makeEdge : String -> List Point -> Edge
makeEdge orientation points =
  let
    -- Rotates q pi/2 clockwise around p
    rotate : Point -> Point -> Point
    rotate p q =
      Point (q.y - p.y + p.x) (q.x - p.x + p.y)

    translate : Point -> Point
    translate p =
      Point.add p (Point 0 200)

    flip p q =
      Point (2*q.x - p.x) (2*q.y - p.y)

    reverse : List Point -> List Point
    reverse ps =
      case ps of
        [p0, p1, p2, p3, p4, p5, p6, p7, p8, p9] ->
          [p9, p8, flip p6 p7, p7, flip p4 p5, p5, flip p2 p3, p3, p1, p0]
        _ ->
          List.reverse ps

    fixOrientation pts =
      case pts of
        p :: ps ->
          case orientation of
            "W" -> List.map (rotate p) <| reverse (p :: ps)
            "S" -> List.map translate <| reverse (p :: ps)
            "E" -> List.map ((rotate p) << translate) (p :: ps)
            _ -> p :: ps
        ps -> ps

  in
  case fixOrientation points of
    [p0, p1, p2, p3, p4, p5, p6, p7, p8, p9] ->
      Curved
        { start = p0
        , b1 = C p1 p2 p3
        , b2 = S p4 p5
        , b3 = S p6 p7
        , b4 = S p8 p9
        }
    [p1, p2] ->
      Flat { start = p1, end = p2 }
    _ ->
      defaultCurvedEdge


bezierToString : Bezier -> String
bezierToString b =
  let
    combine pts =
      String.concat
        <| List.intersperse ", "
        <| List.map Point.toString pts
  in
  case b of
    C p1 p2 p3 ->
      "C " ++ (combine [p1, p2, p3])
    S p1 p2 ->
      "S " ++ (combine [p1, p2])

edgeToString : Edge -> String
edgeToString e =
  case e of
    Curved {start, b1, b2, b3, b4} ->
      List.map bezierToString [b1, b2, b3, b4]
        |> List.intersperse " "
        |> String.concat

    Flat {start, end} ->
      "L " ++ Point.toString start ++ ", " ++ Point.toString end

edgeEndPoint : Edge -> Point
edgeEndPoint edge =
  case edge of
    Curved {b4} ->
      case b4 of
        S _ p2 -> p2
        C _ _ p3 -> p3
    Flat {end} ->
      end

{-| All the EdgePoints are stored in an array where they are indexed in a specific way. All vertical edges are
    enumerated first, going from top left to bottom right. Then the horizontal edges follow. Only internal edges are
    kept in this way - flat edges have an "id" corresponding to -1.

    All the pieces in the puzzle are also ordered in a specific way: piece 0 in the top left corner and piece n in the
    bottom right corner. If we know the width and height (in pieces) of the puzzle, we can therefore deduce the id
    of any specific edge for a specific piece id.

    The eastern edge of the top left corner is the vertical corner with index 0. This is the same as the western edge
    of the second piece:

    getEdgePointsId nx ny 0 "E" == 0
    getEdgePointsId nx ny 1 "W" == 0
-}
getEdgePointsId : Int -> Int -> Int -> String -> Int
getEdgePointsId nx ny pid orientation =
  let
    nv = (nx - 1) * ny
    n = nx * ny
  in
  case orientation of
    "N" ->
      if pid < nx then -1 else pid - nx + nv
    "W" ->
      if (modBy nx pid) == 0 then -1 else pid - (pid // nx) - 1
    "S" ->
      if pid >= n - nx then -1 else pid + nv
    _ ->
      if (modBy nx pid) == (nx - 1) then -1 else pid - (pid // nx)


{-
TODO: Refactor this function to make it readable. In the meanwhile, here's some (hopefully) clarifying words:
Input is a list of piece ids (pids) corresponding to the pieces in a piece group (also the pre-generated array of
points that is used to create the squiggly paths, and nx and ny which are just the number of pieces in the puzzle.

The output is a string describing the (possibly disconnected) clip path of the whole piece group. Note that this
includes potential "holes" in the group, like if you were to finish all the edge pieces, there's one path describing
the outer edge (rectangle) and a different path describing the inside.

The whole thing is a bit tricky, perhaps more than it seems. From the pids and edgePoints array we can obtain the
four edges paths for each of the pieces. But in order to combine them into a single path, we must first remove all
the "inner" edges that we don't want to draw, translate each path to its correct position, and combine them, in the
proper order, to a final path. Consider for example a single piece with north, east, south and western edges. We
can combine those easily into a valid path by concatenating the strings, but if we do it in the wrong order, for example
"north ++ south ++ east ++ west", we end up with a path that clips the wrong way.

Furthermore, each edge is normalized to a coordinate system starting at the origin, but when we combine them they must
be translated to their correct positions first.

The algorithm works like follows (you can read it pretty much from top to bottom in the code, but it's hard to follow):
1. Given a pid (piece ID) and an orientation ("N", "E", "W" or "S"), we can calculate the epid (edge point id) which
   is the index of the corresponding set of edge-points in the edgePoints array. Note that the edgePoints are shared
   between pieces, so that the western edge of piece 0 is the same as the eastern edge of piece 1.
2. Given a pid and a set of edge-points, we can translate the points to where they need to be. We know that the edges
   are normalized to be 200 pixels wide.
3. From the (translated) edge-points, pid and epid, we create a list of (epid, Edge) tuples, one tuple for each edge
   and piece in the piecegroup (so 4*len(pieceGroup.members) tuples, as it were). The Edge is a special
   type which knows how to turn edgePoints + orientation into an svg path. Note that while edgePoints are shared between
   pieces, Edges are not. Think of edgePoints as describing the shape of the edge, while the Edge has a directionality
   which allows svg to draw a connected path.
4. We observe that the edges we want to remove from our collection is precisely the ones where the epid occurs twice!
   We want to filter our list of (epid, Edge) tuples based on this, but in order to do this, we have to count how many
   times the epids occur.
5. But there is one problem! The flat edges don't have a unique epid (it defaults to -1). We need to keep all the flat
   edges, because they are never shared between pieces.
6. Now that we have the unique edges, the next tricky part comes. We essentially want to iterate over all the edges by
   picking any edge at all, turning that into a string, then finding the edge that starts where the previous edge left
   off, turn that one into a string, etc, until we either run out of edges or we are back to where we began. Then we
   have to repeat that process until we run out of edges, one time for each "hole" in the set of pieces.
7. In order to quickly find the next edge, we create a dictionary of edges, where the starting point is the key. Alas,
   Elm doesn't allow Record types to be compared (even when they consist only of comparables!) and custom types also
   can't be made comparable. Frustrating, but luckily we can turn our Point into a string and use that for key.
8. Next problem is a corner case, in which we don't exactly have a "hole" in the pieces, but rather two edges touching,
   forming a path that is not exactly closed, but passes through the same point twice. This leads to a situation where
   we would have two different edges that start at the same point. To circumvent this, we make the dictionary values
   into List Edge.
9. Now we can make a connected path by recursively calling the same function, which takes the next edge from the dict
   and removes the key (unless there were two edges at that key, in which case we only remove the one we took!).
10.The final path is formed by recursively calling the first function until the dict of edges is empty, concatenating
   the resulting strings as we go along. Phew!
-}

pieceGroupCurve pids nx ny edgePoints =
  let
    translate : Int -> EdgePoints -> EdgePoints
    translate pid pts =
      let
        offset = Point
          (200 * (modBy nx pid))
          (200 * (pid // nx))
      in
        List.map (Point.add offset) pts

    points : Int -> Int -> EdgePoints
    points pid epid =
      Array.get epid edgePoints
        |> Maybe.withDefault [Point 0 0, Point 200 0]
        |> translate pid


    epidEdges : List (Int, Edge)
    epidEdges =
      let
        epidEdge pid orientation =
          let
            epid = getEdgePointsId nx ny pid orientation
            edge = makeEdge orientation <| points pid epid
          in
            (epid, edge)

        pidToEdge pid =
          List.map (epidEdge pid) ["N", "E", "S", "W"]
      in
        List.concat <| List.map pidToEdge pids

    epids : List Int
    epids = Tuple.first <| List.unzip epidEdges

    epidCounts : D.Dict Int Int
    epidCounts =
      let
        updateCount c =
          case c of
            Nothing -> Just 1
            Just x -> Just (x+1)
        countEpid epid counts =
          D.update epid updateCount counts
      in
        List.foldl countEpid D.empty epids

    uniqueEdges : List Edge
    uniqueEdges =
      let
        filterAwayInnerEdges (epid, edge) edges =
          if (isBorder epid) || (edgeCount epid == 1) then
            edge :: edges
          else
            edges

        isBorder epid =
          epid < 0
        edgeCount epid =
          D.get epid epidCounts |> Maybe.withDefault 0
      in
        List.foldl filterAwayInnerEdges [] epidEdges

    edgeDict : D.Dict String (List Edge)
    edgeDict =
      let
        insertEdge edge dict =
          D.update (edgeKey edge) (update edge) dict

        edgeKey : Edge -> String
        edgeKey e =
          case e of
            Curved c -> Point.toString c.start
            Flat f -> Point.toString f.start

        update : Edge -> Maybe (List Edge) -> Maybe (List Edge)
        update newEdge existingEdges =
          case existingEdges of
            Nothing -> Just [newEdge]
            Just edges -> Just (newEdge :: edges)
      in
        List.foldl insertEdge D.empty uniqueEdges

    connectedPath : String -> String -> D.Dict String (List Edge) -> (String, D.Dict String (List Edge))
    connectedPath str start dict =
      let
        update new old =
          case old of
            Nothing -> Nothing
            Just _ -> Just new
        recurse e =
          connectedPath (str ++ edgeToString e) (Point.toString (edgeEndPoint e))
      in
      case D.get start dict of
        Just (edge :: []) -> recurse edge <| D.remove start dict
        Just (edge :: edges) -> recurse edge <| D.update start (update edges) dict
        _ -> (str, dict)

    allConnectedPaths : String -> D.Dict String (List Edge) -> String
    allConnectedPaths str dict =
      case List.head (D.keys dict) of
        Just start ->
          Util.applyTuple allConnectedPaths <| connectedPath (str ++ "M " ++ start) start dict
        Nothing ->
          str
  in
    allConnectedPaths "" edgeDict