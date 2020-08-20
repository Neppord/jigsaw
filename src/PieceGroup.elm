module PieceGroup exposing (PieceGroup, merge)

import Point exposing (Point)
import Set as S


type alias PieceGroup =
    { id : Int
    , members : List Int
    , neighbours : S.Set Int
    , position : Point
    , isSelected : Bool
    , zlevel : Int
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
        , zlevel = a.zlevel
    }