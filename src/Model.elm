module Model exposing
    ( Box
    , Key(..)
    , Keyboard
    , Model
    , Msg(..)
    , Selected(..)
    , SelectionBox(..)
    , boxBottomRight
    , boxTopLeft
    , defaultPieceGroup
    )

import Dict as D
import Edge exposing (Edge)
import JigsawImage
    exposing
        ( JigsawImage
        , PieceGroup
        )
import Point exposing (Point)
import Random
import Set as S


type Msg
    = MouseDown Point Keyboard
    | MouseMove Point
    | MouseUp
    | Scramble
    | KeyChanged Bool Key


type alias Keyboard =
    { shift : Bool
    , ctrl : Bool
    }


type alias Model =
    { cursor : Maybe Point
    , pieceGroups : D.Dict Int PieceGroup
    , selected : Selected
    , maxZLevel : Int
    , image : JigsawImage
    , width : Int
    , height : Int
    , snapDistance : Float
    , selectionBox : SelectionBox
    , seed : Random.Seed
    , edges : List (List Edge)
    , visibleGroups : S.Set Int
    , keyboard : Keyboard
    }


type SelectionBox
    = Normal Box
    | Inverted Box
    | NullBox


type alias Box =
    { staticCorner : Point
    , movingCorner : Point
    , selectedIds : S.Set Int
    }


type Selected
    = Multiple
    | Single Int
    | NullSelection


type Key
    = Number Int
    | Control
    | Shift
    | Other


boxTopLeft : Box -> Point
boxTopLeft box =
    Point
        (min box.staticCorner.x box.movingCorner.x)
        (min box.staticCorner.y box.movingCorner.y)


boxBottomRight : Box -> Point
boxBottomRight box =
    Point
        (max box.staticCorner.x box.movingCorner.x)
        (max box.staticCorner.y box.movingCorner.y)



-- Until I figure out how to handle index out of bounds
-- exceptions more elegantly


defaultPieceGroup : PieceGroup
defaultPieceGroup =
    { position = Point 0 0
    , isSelected = False
    , zlevel = -1
    , id = -10
    , neighbours = S.empty
    , members = []
    , visibilityGroup = -1
    }
