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
    rotate : Point -> Point -> Point
    rotate p q =
      Point (q.y - p.y + p.x) (q.x - p.x + p.y)
--      Point (q.y + p.x) (q.x - p.x)
--      Point p.y p.x

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
      let
        curve =
          List.map bezierToString [b1, b2, b3, b4]
            |> List.intersperse " "
            |> String.concat
      in
--        "M " ++ Point.toString start ++ curve
        curve
    Flat {start, end} ->
      "L " ++ Point.toString start ++ ", " ++ Point.toString end

pieceCurveFromPieceId : Int -> Int -> Int -> Array.Array EdgePoints -> String
pieceCurveFromPieceId nx ny id edgePoints =
  let
    edge : String -> Edge
    edge orientation = getEdge orientation nx ny id edgePoints
  in
    List.map (edge >> edgeToString) ["N", "E", "S", "W"]
      |> String.concat

getEdge : String -> Int -> Int -> Int -> Array.Array EdgePoints-> Edge
getEdge orientation nx ny id edgePoints =
  let
    nv = (nx - 1) * ny
    n = nx * ny
    index =
      case orientation of
        "N" ->
          if id < nx then -1 else id - nx + nv
        "W" ->
          if (modBy nx id) == 0 then -1 else id - (id // nx) - 1
        "S" ->
          if id >= n - nx then -1 else id + nv
        _ ->
          if (modBy nx id) == (nx - 1) then -1 else id - (id // nx)
    points =
      Array.get index edgePoints
        |> Maybe.withDefault [Point 0 0, Point 200 0]
  in
    makeEdge orientation points

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
    pidToEpid : Int -> String -> Int
    pidToEpid pid orientation =
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

    fixTranslation : Int -> EdgePoints -> EdgePoints
    fixTranslation pid pts =
      List.map (Point.add (Point (200 * (modBy nx pid)) (200 * (pid // nx)))) pts

    points : Int -> Int -> EdgePoints
    points pid epid =
      Array.get epid edgePoints
        |> Maybe.withDefault [Point 0 0, Point 200 0]
        |> fixTranslation pid



    pidToEdge : Int -> List (Int, Edge)
    pidToEdge pid =
      let
        epid =
          pidToEpid pid
        epidEdge o =
          (epid o, makeEdge o <| points pid (epid o))
      in
        List.map epidEdge ["N", "E", "S", "W"]

    epidEdges : List (Int, Edge)
    epidEdges =
      List.concat <| List.map pidToEdge pids

    (epids, _) = List.unzip epidEdges

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

    uniqueEdges =
      List.filter (\(epid, _) -> epid < 0 || (Maybe.withDefault 0 (D.get epid epidCounts) == 1)) epidEdges

    edgeKey : Edge -> String
    edgeKey e =
      case e of
        Curved c -> Point.toString c.start
        Flat f -> Point.toString f.start

    edgeDict =
      let
        update e value =
          case value of
            Nothing -> Just [e]
            Just es -> Just (e :: es)
      in
      List.foldl (\(_, edge) dict -> D.update (edgeKey edge) (update edge) dict) D.empty uniqueEdges

    endPoint e =
      case e of
        Curved {b4} ->
          case b4 of
            S _ p2 -> p2
            C _ _ p3 -> p3
        Flat {end} ->
          end

    connectedPath start dict str =
      let
        update new old =
          case old of
            Nothing -> Nothing
            Just _ -> Just new
      in
      case D.get start dict of
        Nothing -> (dict, str)
        Just (e :: []) -> connectedPath (Point.toString (endPoint e)) (D.remove start dict) (str ++ edgeToString e)
        Just (e :: es) -> connectedPath (Point.toString (endPoint e)) (D.update start (update es) dict) (str ++ edgeToString e)
        Just [] -> (dict, str)

    entireClipPath dict str =
      case List.head (D.keys dict) of
        Just start ->
          Util.applyTuple entireClipPath (connectedPath start dict (str ++ "M " ++ start))
        Nothing ->
          str
  in
    entireClipPath edgeDict ""