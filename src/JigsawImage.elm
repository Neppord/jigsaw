module JigsawImage exposing
    ( JigsawImage
    , pieceIdToOffset
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
