module PieceGroup exposing (PieceGroup)

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
