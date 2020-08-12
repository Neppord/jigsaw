module SvgUtil exposing
    ( bezierToSvg
    , edgeToSvg
    , edgesToSvg
    , pieceToSvg
    , pointsToSvg
    )

import Edge
    exposing
        ( Bezier(..)
        , Edge(..)
        , Orientation(..)
        )
import Point exposing (Point)


bezierToSvg : Bezier -> String
bezierToSvg b =
    case b of
        C p1 p2 p3 ->
            "C " ++ pointsToSvg [ p1, p2, p3 ]

        S p1 p2 ->
            "S " ++ pointsToSvg [ p1, p2 ]


edgeToSvg : Edge -> String
edgeToSvg e =
    case e of
        Curved { b1, b2, b3, b4 } ->
            List.map bezierToSvg [ b1, b2, b3, b4 ]
                |> String.join " "

        Flat { a, b } ->
            "L " ++ pointsToSvg [ a, b ]


edgesToSvg : List Edge -> String
edgesToSvg edges =
    edges
        |> List.map edgeToSvg
        |> String.concat


pointsToSvg : List Point -> String
pointsToSvg pts =
    pts
        |> List.map Point.toString
        |> String.join ", "


pieceToSvg : List Edge -> String
pieceToSvg edges =
    "M 0 0" ++ edgesToSvg edges
