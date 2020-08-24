module PieceGroup exposing (PieceGroup, move, distance, merge)

import Point exposing (Point)
import Set as S


type alias PieceGroup =
    { id : Int
    , members : List Int
    , neighbours : S.Set Int
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
            S.diff (S.union b.neighbours a.neighbours) (S.fromList newMembers)
    in
    { b
        | isSelected = False
        , members = newMembers
        , neighbours = newNeighbours
    }


distance : PieceGroup -> PieceGroup -> Float
distance from to =
    Point.dist from.position to.position

move : Point -> PieceGroup -> PieceGroup
move offset pg = {pg | position = Point.add offset pg.position}