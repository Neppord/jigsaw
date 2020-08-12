module Edge exposing
    ( Bezier(..)
    , Edge(..)
    , EdgePoints
    , getEdge
    , makeEdgePoints
    , Orientation(..)
    )

import Array
import Point exposing (Point)
import Random


rApply : Random.Generator (a -> b) -> Random.Generator a -> Random.Generator b
rApply toApply on =
    toApply
        |> Random.andThen (\f -> Random.map f on)


randomOffsets : Random.Generator (List Point)
randomOffsets =
    Random.map (\list -> list ++ [ Point 0 0 ]) (Point.randomPoints 8 -5 5 -5 5)


flipY : Random.Generator (List Point -> List Point)
flipY =
    Random.uniform (Point 1 1) [ Point 1 -1 ]
        |> Random.map Point.dot
        |> Random.map List.map


generateEdgePoints : Int -> Random.Generator (Array.Array EdgePoints)
generateEdgePoints n =
    defaultPoints
        |> Random.constant
        |> Random.map2 (List.map2 Point.add) randomOffsets
        |> rApply flipY
        |> Random.list n
        |> Random.map Array.fromList


makeEdgePoints : Int -> Random.Seed -> ( Array.Array EdgePoints, Random.Seed )
makeEdgePoints n seed =
    Random.step (generateEdgePoints n) seed


type Edge
    = Curved { b1 : Bezier, b2 : Bezier, b3 : Bezier, b4 : Bezier }
    | Flat { a : Point, b : Point }


type alias EdgePoints =
    List Point


type Bezier
    = C Point Point Point
    | S Point Point


defaultCurvedEdge : Edge
defaultCurvedEdge =
    Curved
        { b1 = C (Point 50 20) (Point 100 25) (Point 80 0)
        , b2 = S (Point 70 -40) (Point 100 -40)
        , b3 = S (Point 140 -25) (Point 120 0)
        , b4 = S (Point 150 20) (Point 200 0)
        }


defaultPoints : List Point
defaultPoints =
    [ Point 50 20
    , Point 100 25
    , Point 80 0
    , Point 70 -40
    , Point 100 -40
    , Point 140 -25
    , Point 120 0
    , Point 150 20
    , Point 200 0
    ]


makeEdge : Orientation -> List Point -> Edge
makeEdge orientation points =
    let
        rotate : Point -> Point
        rotate p =
            Point p.y p.x

        translate : Point -> Point
        translate p =
            Point.add p (Point 0 200)

        flip p q =
            Point (2 * q.x - p.x) (2 * q.y - p.y)

        reverse : List Point -> List Point
        reverse ps =
            case ps of
                [ p1, p2, p3, p4, p5, p6, p7, p8, _ ] ->
                    [ p8, flip p6 p7, p7, flip p4 p5, p5, flip p2 p3, p3, p1, Point 0 0 ]

                _ ->
                    List.reverse ps

        fixOrientation ps =
            case orientation of
                West ->
                    List.map rotate <| reverse ps

                South ->
                    List.map translate <| reverse ps

                East ->
                    List.map (rotate << translate) ps

                North ->
                    ps
    in
    case fixOrientation points of
        [ p1, p2, p3, p4, p5, p6, p7, p8, p9 ] ->
            Curved
                { b1 = C p1 p2 p3
                , b2 = S p4 p5
                , b3 = S p6 p7
                , b4 = S p8 p9
                }

        [ p1, p2 ] ->
            Flat { a = p1, b = p2 }

        _ ->
            defaultCurvedEdge


type Orientation
    = North
    | East
    | South
    | West


getEdge : Orientation -> Int -> Int -> Int -> Array.Array EdgePoints -> Edge
getEdge orientation nx ny id edgePoints =
    let
        nv =
            (nx - 1) * ny

        n =
            nx * ny

        index =
            case orientation of
                North ->
                    if id < nx then
                        -1

                    else
                        id - nx + nv

                West ->
                    if modBy nx id == 0 then
                        -1

                    else
                        id - (id // nx) - 1

                South ->
                    if id >= n - nx then
                        -1

                    else
                        id + nv

                East ->
                    if modBy nx id == (nx - 1) then
                        -1

                    else
                        id - (id // nx)

        points =
            Array.get index edgePoints
                |> Maybe.withDefault [ Point 0 0, Point 200 0 ]
    in
    makeEdge orientation points
