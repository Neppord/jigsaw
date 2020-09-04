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
    , shouldBeMerged
    )

import JigsawImage exposing (JigsawImage)
import Point exposing (Point)
import Set exposing (Set)


type alias ID =
    ( Int, Int )


type alias Piece =
    { id : ID
    , offset : Point
    }


type alias PieceGroup =
    { id : ID
    , members : List Piece
    , neighbours : Set ID
    , position : Point
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
    in
    { b
        | isSelected = False
        , members = newMembers
        , neighbours = newNeighbours
    }


shouldBeMerged : Float -> PieceGroup -> PieceGroup -> Bool
shouldBeMerged snapDistance one other =
    let
        otherIds =
            Set.fromList <| List.map .id other.members
    in
    (distance other one < snapDistance)
        && ((Set.size <| Set.intersect otherIds one.neighbours) > 0)


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
    in
    { position = Point.sub pos offset
    , isSelected = False
    , id = id
    , members = [ Piece id offset ]
    , neighbours = neighbours id
    , visibilityGroup = -1
    }


isPieceGroupInsideBox : JigsawImage -> Point -> Point -> PieceGroup -> Bool
isPieceGroupInsideBox image boxTL boxBR pieceGroup =
    List.any (JigsawImage.isPieceInsideBox image pieceGroup.position boxTL boxBR) <|
        List.map .id pieceGroup.members


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
