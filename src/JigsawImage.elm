module JigsawImage exposing (..)

import Dict as D
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


createPieceGroups : JigsawImage -> List Point -> List Int -> D.Dict Int PieceGroup
createPieceGroups image points levels =
    let
        nx =
            image.xpieces

        ny =
            image.ypieces

        n =
            nx * ny

        range =
            List.range 0 (n - 1)

        positions =
            if List.length points < n then
                List.map (pieceIdToOffset image) range

            else
                points

        zlevels =
            if List.length levels < n then
                range

            else
                levels

        neighbourOffsets =
            [ -nx, -1, 1, nx ]

        possibleNeighbours i =
            List.map ((+) i) neighbourOffsets

        isRealNeighbour i x =
            x
                >= 0
                && x
                < n
                && Point.taxiDist
                    (pieceIdToPoint i image.xpieces)
                    (pieceIdToPoint x image.xpieces)
                == 1

        onePieceGroup i pos zlevel =
            ( i
            , { position = Point.sub pos (pieceIdToOffset image i)
              , isSelected = False
              , id = i
              , zlevel = zlevel
              , members = [ i ]
              , neighbours = S.filter (isRealNeighbour i) <| S.fromList (possibleNeighbours i)
              , visibilityGroup = -1
              }
            )
    in
    D.fromList <| List.map3 onePieceGroup range positions zlevels


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
