module JigsawImage exposing
    ( JigsawImage
    , isPieceInsideBox
    , isPointInsidePiece
    , pieceIdToOffset
    , pieceIdToPoint
    , shufflePiecePositions
    )

import Point exposing (Point)
import Random


type alias JigsawImage =
    { path : String
    , width : Int
    , height : Int
    , xpieces : Int
    , ypieces : Int
    , pieceWidth : Int
    , pieceHeight : Int
    }


shufflePiecePositions : Int -> Int -> JigsawImage -> Random.Generator (List Point)
shufflePiecePositions w h image =
    let
        n =
            image.xpieces * image.ypieces

        xmin =
            0

        xmax =
            w - image.width // image.xpieces

        ymin =
            0

        ymax =
            h - image.height // image.ypieces
    in
    Point.randomPoints n xmin xmax ymin ymax


pieceIdToOffset : JigsawImage -> ( Int, Int ) -> Point
pieceIdToOffset image ( x, y ) =
    Point (x * image.pieceWidth) (y * image.pieceHeight)


isPieceInsideBox : JigsawImage -> Point -> Point -> Point -> ( Int, Int ) -> Bool
isPieceInsideBox image pos boxTL boxBR id =
    let
        pieceTL =
            Point.add pos <| pieceIdToOffset image id

        pieceBR =
            Point.add pieceTL <| Point image.pieceWidth image.pieceHeight
    in
    (pieceTL.x <= boxBR.x)
        && (pieceTL.y <= boxBR.y)
        && (pieceBR.x >= boxTL.x)
        && (pieceBR.y >= boxTL.y)


isPointInsidePiece : JigsawImage -> Point -> Point -> ( Int, Int ) -> Bool
isPointInsidePiece image point pos id =
    let
        pieceTL =
            Point.add pos <| pieceIdToOffset image id

        pieceBR =
            Point.add pieceTL <| Point image.pieceWidth image.pieceHeight
    in
    (pieceTL.x <= point.x)
        && (pieceTL.y <= point.y)
        && (pieceBR.x >= point.x)
        && (pieceBR.y >= point.y)


pieceIdToPoint : Int -> Int -> Point
pieceIdToPoint id xpieces =
    Point (modBy xpieces id) (id // xpieces)
