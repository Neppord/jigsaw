module PieceGroup exposing
    ( ID
    , Piece
    , PieceGroup
    , createPieceGroup
    , createPieceGroups
    , deselect
    , distance
    , genIds
    , isPieceGroupInsideBox
    , isPointInsidePieceGroup
    , merge
    , move
    , select
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
    { id : ID
    , members : List Piece
    , neighbours : Set ID
    , position : Point
    , minOffset : Point
    , maxOffset : Point
    , isSelected : Bool
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
    { id = a.id
    , minOffset = minOffset
    , maxOffset = maxOffset
    , isSelected = False
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


select : PieceGroup -> PieceGroup
select x =
    { x | isSelected = True }


deselect : PieceGroup -> PieceGroup
deselect x =
    { x | isSelected = False }


createPieceGroup : JigsawImage -> ID -> Point -> PieceGroup
createPieceGroup image id pos =
    let
        offset =
            JigsawImage.pieceIdToOffset image id

        size =
            Point image.pieceWidth image.pieceHeight
    in
    { position = Point.sub pos offset
    , isSelected = False
    , id = id
    , members = [ Piece id offset size ]
    , neighbours = neighbours id
    , visibilityGroup = -1
    , minOffset = offset
    , maxOffset = Point.add size offset
    }


isPieceGroupInsideBox : Point -> Point -> PieceGroup -> Bool
isPieceGroupInsideBox boxTL boxBR pieceGroup =
    List.any
        (isPieceInsideBox pieceGroup.position boxTL boxBR)
        pieceGroup.members


isPieceInsideBox : Point -> Point -> Point -> Piece -> Bool
isPieceInsideBox pos boxTL boxBR piece =
    let
        pieceTL =
            Point.add pos piece.offset

        pieceBR =
            Point.add pieceTL piece.size
    in
    (pieceTL.x <= boxBR.x)
        && (pieceTL.y <= boxBR.y)
        && (pieceBR.x >= boxTL.x)
        && (pieceBR.y >= boxTL.y)


isPointInsidePieceGroup : Set Int -> JigsawImage -> Point -> PieceGroup -> Bool
isPointInsidePieceGroup visibleGroups image point pieceGroup =
    Set.member pieceGroup.visibilityGroup visibleGroups
        && (List.any (JigsawImage.isPointInsidePiece image point pieceGroup.position) <|
                List.map .id pieceGroup.members
           )


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
