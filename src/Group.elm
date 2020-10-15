module Group exposing
    ( Group
    , ID
    , Piece
    , createGroup
    , createGroups
    , distance
    , genIds
    , isPointInsideGroup
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


type alias Group =
    { id : ID
    , members : List Piece
    , neighbours : Set ID
    , position : Point
    , minOffset : Point
    , maxOffset : Point
    , visibilityGroup : Int
    }


merge : Group -> Group -> Group
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
    , members = newMembers
    , neighbours = newNeighbours
    , visibilityGroup = a.visibilityGroup
    , position = a.position
    }


distance : Group -> Group -> Float
distance from to =
    Point.dist from.position to.position


move : Point -> Group -> Group
move offset pg =
    { pg | position = Point.add offset pg.position }


createGroup : JigsawImage -> ID -> Point -> Group
createGroup image id pos =
    let
        offset =
            JigsawImage.pieceIdToOffset image id

        size =
            Point image.pieceWidth image.pieceHeight
    in
    { id = id
    , position = Point.sub pos offset
    , members = [ Piece id offset size ]
    , neighbours = neighbours id
    , visibilityGroup = -1
    , minOffset = offset
    , maxOffset = Point.add size offset
    }


isPointInsideGroup : Point -> Group -> Bool
isPointInsideGroup point pieceGroup =
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


createGroups : JigsawImage -> List Point -> List Group
createGroups image points =
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
    List.map2 (createGroup image) ids positions


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
