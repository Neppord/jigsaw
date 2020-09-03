module PieceGroup exposing (PieceGroup, deselect, select, distance, merge, move, shouldBeMerged)

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
select x = {x | isSelected = True}
deselect : PieceGroup -> PieceGroup
deselect x = {x | isSelected = False}
