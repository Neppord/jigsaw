module PieceGroup exposing
    ( ID
    , Piece
    , PieceGroup
    , createPieceGroup
    , createPieceGroups
    , distance
    , genIds
    , isPointInsidePieceGroup
    , merge
    , move
    )

import JigsawImage exposing (JigsawImage)
import Point exposing (Point)
import Set exposing (Set)


type alias ID =
    ( Int, Int )


type alias Piece =
    { id : ID
    , offset : Point
    , size : Point
    }


type alias PieceGroup =
    { members : List Piece
    , neighbours : Set ID
    , position : Point
    , minOffset : Point
    , maxOffset : Point
    , visibilityGroup : Int
    }


merge : PieceGroup -> PieceGroup -> PieceGroup
merge a b =
    let
        newMembers =
            b.members ++ a.members

        newNeighbours =
            Set.diff
                (Set.union b.neighbours a.neighbours)
                (Set.fromList <| List.map .id newMembers)

        minOffset =
            Point
                (min a.minOffset.x b.minOffset.x)
                (min a.minOffset.y b.minOffset.y)

        maxOffset =
            Point
                (max a.maxOffset.x b.maxOffset.x)
                (max a.maxOffset.y b.maxOffset.y)
    in
    { minOffset = minOffset
    , maxOffset = maxOffset
    , members = newMembers
    , neighbours = newNeighbours
    , visibilityGroup = a.visibilityGroup
    , position = a.position
    }


distance : PieceGroup -> PieceGroup -> Float
distance from to =
    Point.dist from.position to.position


move : Point -> PieceGroup -> PieceGroup
move offset pg =
    { pg | position = Point.add offset pg.position }


createPieceGroup : JigsawImage -> ID -> Point -> PieceGroup
createPieceGroup image id pos =
    let
        offset =
            JigsawImage.pieceIdToOffset image id

        size =
            Point image.pieceWidth image.pieceHeight
    in
    { position = Point.sub pos offset
    , members = [ Piece id offset size ]
    , neighbours = neighbours id
    , visibilityGroup = -1
    , minOffset = offset
    , maxOffset = Point.add size offset
    }


isPointInsidePieceGroup : Point -> PieceGroup -> Bool
isPointInsidePieceGroup point pieceGroup =
    let
        relativePosition =
            Point.sub point pieceGroup.position
    in
    List.any
        (isPointInsidePiece relativePosition)
        pieceGroup.members


isPointInsidePiece : Point -> Piece -> Bool
isPointInsidePiece point piece =
    let
        pieceTL =
            piece.offset

        pieceBR =
            Point.add piece.offset piece.size
    in
    (pieceTL.x <= point.x)
        && (pieceTL.y <= point.y)
        && (pieceBR.x >= point.x)
        && (pieceBR.y >= point.y)


createPieceGroups : JigsawImage -> List Point -> List PieceGroup
createPieceGroups image points =
    let
        numberOfPieces =
            image.xpieces * image.ypieces

        ids : List ID
        ids =
            genIds image.xpieces image.ypieces

        positions =
            if List.length points < numberOfPieces then
                List.map (JigsawImage.pieceIdToOffset image) ids

            else
                points
    in
    List.map2 (createPieceGroup image) ids positions


neighbours : ID -> Set ID
neighbours ( x, y ) =
    Set.fromList
        [ ( x - 1, y )
        , ( x, y - 1 )
        , ( x + 1, y )
        , ( x, y + 1 )
        ]


genIds : Int -> Int -> List ID
genIds w h =
    List.range 0 (w - 1)
        |> List.concatMap
            (\x ->
                List.range 0 (h - 1)
                    |> List.map (Tuple.pair x)
            )
