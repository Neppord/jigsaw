module JigsawImage exposing (..)

import Dict as D
import Math.Matrix4 exposing (identity)
import Point exposing (Point)
import Random
import Set as S


type alias JigsawImage =
    { path : String
    , width : Int
    , height : Int
    , xpieces : Int
    , ypieces : Int
    , scale : Float
    }


type alias PieceGroup =
    { id : Int
    , members : List Int
    , neighbours : S.Set Int
    , position : Point
    , isSelected : Bool
    , zlevel : Int
    , visibilityGroup : Int
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


createPieceGroup : JigsawImage -> Int -> Point -> Int -> PieceGroup
createPieceGroup image id pos zlevel =
    let
        isRealNeighbour i x =
            x
                >= 0
                && x
                < (image.xpieces * image.ypieces)
                && Point.taxiDist
                    (pieceIdToPoint i image.xpieces)
                    (pieceIdToPoint x image.xpieces)
                == 1

        possibleNeighbours i =
            [ i - image.xpieces, i - 1, i + 1, i + image.xpieces ]

        neighbours =
            possibleNeighbours id
                |> S.fromList
                |> S.filter (isRealNeighbour id)

        position =
            Point.sub pos (pieceIdToOffset image id)
    in
    { position = position
    , isSelected = False
    , id = id
    , zlevel = zlevel
    , members = [ id ]
    , neighbours = neighbours
    , visibilityGroup = -1
    }


createPieceGroups : JigsawImage -> List Point -> List Int -> D.Dict Int PieceGroup
createPieceGroups image points levels =
    let
        numberOfPieces =
            image.xpieces * image.ypieces

        ids =
            List.range 0 (numberOfPieces - 1)

        positions =
            if List.length points < numberOfPieces then
                List.map (pieceIdToOffset image) ids

            else
                points

        zlevels =
            if List.length levels < numberOfPieces then
                ids

            else
                levels
    in
    List.map3 (createPieceGroup image) ids positions zlevels
        |> List.map2 Tuple.pair ids
        |> D.fromList


pieceIdToOffset : JigsawImage -> Int -> Point
pieceIdToOffset image id =
    let
        pieceWidth =
            floor <| image.scale * toFloat (image.width // image.xpieces)

        pieceHeight =
            floor <| image.scale * toFloat (image.height // image.ypieces)
    in
    Point.dot
        (pieceIdToPoint id image.xpieces)
        (Point pieceWidth pieceHeight)


isPieceInsideBox : JigsawImage -> Point -> Point -> Point -> Int -> Bool
isPieceInsideBox image pos boxTL boxBR id =
    let
        pieceWidth =
            floor <| image.scale * toFloat (image.width // image.xpieces)

        pieceHeight =
            floor <| image.scale * toFloat (image.height // image.ypieces)

        pieceTL =
            Point.add pos <| pieceIdToOffset image id

        pieceBR =
            Point.add pieceTL <| Point pieceWidth pieceHeight
    in
    (pieceTL.x <= boxBR.x)
        && (pieceTL.y + 100 <= boxBR.y)
        && (pieceBR.x >= boxTL.x)
        && (pieceBR.y + 100 >= boxTL.y)


isPieceGroupInsideBox : JigsawImage -> Point -> Point -> PieceGroup -> Bool
isPieceGroupInsideBox image boxTL boxBR pieceGroup =
    List.any (isPieceInsideBox image pieceGroup.position boxTL boxBR) pieceGroup.members


isPointInsidePiece : JigsawImage -> Point -> Point -> Int -> Bool
isPointInsidePiece image point pos id =
    let
        pieceWidth =
            floor <| image.scale * toFloat (image.width // image.xpieces)

        pieceHeight =
            floor <| image.scale * toFloat (image.height // image.ypieces)

        pieceTL =
            Point.add pos <| pieceIdToOffset image id

        pieceBR =
            Point.add pieceTL <| Point pieceWidth pieceHeight
    in
    (pieceTL.x <= point.x)
        && (pieceTL.y + 100 <= point.y)
        && (pieceBR.x >= point.x)
        && (pieceBR.y + 100 >= point.y)


isPointInsidePieceGroup visibleGroups image point pieceGroup =
    S.member pieceGroup.visibilityGroup visibleGroups
        && List.any (isPointInsidePiece image point pieceGroup.position) pieceGroup.members


pieceIdToPoint : Int -> Int -> Point
pieceIdToPoint id xpieces =
    Point (modBy xpieces id) (id // xpieces)
