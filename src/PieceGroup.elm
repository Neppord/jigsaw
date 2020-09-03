module PieceGroup exposing (PieceGroup, createPieceGroup, createPieceGroups, deselect, distance, isPieceGroupInsideBox, isPointInsidePieceGroup, merge, move, select, shouldBeMerged)

import JigsawImage exposing (JigsawImage)
import Point exposing (Point)
import Set exposing (Set)


type alias PieceGroup =
    { id : Int
    , members : List Int
    , neighbours : Set Int
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
            Set.diff (Set.union b.neighbours a.neighbours) (Set.fromList newMembers)
    in
    { b
        | isSelected = False
        , members = newMembers
        , neighbours = newNeighbours
    }


shouldBeMerged : Float -> PieceGroup -> PieceGroup -> Bool
shouldBeMerged snapDistance one other =
    distance other one
        < snapDistance
        && (Set.size <| Set.intersect (Set.fromList other.members) one.neighbours)
        > 0


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


createPieceGroup : JigsawImage -> Int -> Point -> PieceGroup
createPieceGroup image id pos =
    let
        isRealNeighbour i x =
            x
                >= 0
                && x
                < (image.xpieces * image.ypieces)
                && Point.taxiDist
                    (JigsawImage.pieceIdToPoint i image.xpieces)
                    (JigsawImage.pieceIdToPoint x image.xpieces)
                == 1

        possibleNeighbours i =
            [ i - image.xpieces, i - 1, i + 1, i + image.xpieces ]

        neighbours =
            possibleNeighbours id
                |> Set.fromList
                |> Set.filter (isRealNeighbour id)

        position =
            Point.sub pos (JigsawImage.pieceIdToOffset image id)
    in
    { position = position
    , isSelected = False
    , id = id
    , members = [ id ]
    , neighbours = neighbours
    , visibilityGroup = -1
    }


isPieceGroupInsideBox : JigsawImage -> Point -> Point -> PieceGroup -> Bool
isPieceGroupInsideBox image boxTL boxBR pieceGroup =
    List.any (JigsawImage.isPieceInsideBox image pieceGroup.position boxTL boxBR) pieceGroup.members


isPointInsidePieceGroup : Set Int -> JigsawImage -> Point -> PieceGroup -> Bool
isPointInsidePieceGroup visibleGroups image point pieceGroup =
    Set.member pieceGroup.visibilityGroup visibleGroups
        && List.any (JigsawImage.isPointInsidePiece image point pieceGroup.position) pieceGroup.members


createPieceGroups : JigsawImage -> List Point -> List PieceGroup
createPieceGroups image points =
    let
        numberOfPieces =
            image.xpieces * image.ypieces

        ids =
            List.range 0 (numberOfPieces - 1)

        positions =
            if List.length points < numberOfPieces then
                List.map (JigsawImage.pieceIdToOffset image) ids

            else
                points
    in
    List.map2 (createPieceGroup image) ids positions
