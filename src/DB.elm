module DB exposing
    ( DB
    , boxSelect
    , clickedPieceGroup
    , findBy
    , getSelected
    , getUnSelected
    , height
    , makeDb
    , map
    , modify
    , modifyBy
    , modifySelected
    , optimalHeight
    , size
    , snap
    )

import Drag
import JigsawImage exposing (JigsawImage)
import KD.Match exposing (Match(..))
import KDDict exposing (KDDict, MatchKey)
import PieceGroup exposing (PieceGroup)
import Point exposing (Point)
import Set
import UI


type alias DB =
    KDDict Int PieceGroup


type alias DbKey =
    KDDict.Key Int


size : DB -> Int
size =
    KDDict.size


height : DB -> Int
height =
    KDDict.height


optimalHeight : DB -> Int
optimalHeight db =
    db
        |> height
        |> toFloat
        |> logBase 2
        |> floor


keyBool : Bool -> Int
keyBool v =
    if v then
        1

    else
        0


makeKey : PieceGroup -> DbKey
makeKey { id, isSelected, position, minOffset, maxOffset } =
    let
        position_ =
            position
                |> Point.toPair

        x =
            ( position.x + minOffset.x, position.x + maxOffset.x )

        y =
            ( position.y + minOffset.y, position.y + maxOffset.y )
    in
    KDDict.key (keyBool isSelected)
        |> KDDict.addCoordinateAxis position_
        |> KDDict.addCoordinateAxis x
        |> KDDict.addCoordinateAxis y


matchWithin : { x : Int, y : Int, w : Int, h : Int } -> MatchKey Int
matchWithin { x, y, w, h } =
    KDDict.key Anything
        |> KDDict.addCoordinateAxis
            ( WithinRange x (x + w), WithinRange y (y + h) )
        |> KDDict.addCoordinateAxis ( Anything, Anything )
        |> KDDict.addCoordinateAxis ( Anything, Anything )


matchBox : { x : Int, y : Int, w : Int, h : Int } -> MatchKey Int
matchBox { x, y, w, h } =
    KDDict.key Anything
        |> KDDict.addCoordinateAxis ( Anything, Anything )
        |> KDDict.addCoordinateAxis ( LargerThan x, SmallerThan (x + w) )
        |> KDDict.addCoordinateAxis ( LargerThan y, SmallerThan (y + h) )


matchPoint : Point -> MatchKey Int
matchPoint { x, y } =
    let
        radius =
            1
    in
    KDDict.key Anything
        |> KDDict.addAxis Anything
        |> KDDict.addAxis Anything
        |> KDDict.addCoordinateAxis
            ( SmallerThan (x + radius), LargerThan (x - radius) )
        |> KDDict.addCoordinateAxis
            ( SmallerThan (y + radius), LargerThan (y - radius) )


makeDb : List PieceGroup -> DB
makeDb list =
    KDDict.fromListBy makeKey list


order : List PieceGroup -> List PieceGroup
order =
    List.sortBy (\pg -> ( keyBool pg.isSelected, pg.id ))


getSelected : DB -> List PieceGroup
getSelected db =
    db
        |> KDDict.findMatching
            (KDDict.key (EqualTo (keyBool True))
                |> KDDict.addCoordinateAxis ( Anything, Anything )
                |> KDDict.addCoordinateAxis ( Anything, Anything )
                |> KDDict.addCoordinateAxis ( Anything, Anything )
            )
        |> order


getUnSelected : DB -> List PieceGroup
getUnSelected db =
    db
        |> KDDict.findMatching
            (KDDict.key (EqualTo (keyBool False))
                |> KDDict.addCoordinateAxis ( Anything, Anything )
                |> KDDict.addCoordinateAxis ( Anything, Anything )
                |> KDDict.addCoordinateAxis ( Anything, Anything )
            )
        |> order


modifySelected : (PieceGroup -> PieceGroup) -> DB -> DB
modifySelected f db =
    let
        modified =
            List.map f (getSelected db)
    in
    makeDb (modified ++ getUnSelected db)


all : DB -> List PieceGroup
all db =
    db
        |> KDDict.toList
        |> List.map Tuple.second


modify : PieceGroup.ID -> (PieceGroup -> PieceGroup) -> DB -> DB
modify id action db =
    let
        toMap x =
            if x.id == id then
                action x

            else
                x
    in
    makeDb (List.map toMap (all db))


modifyBy : (PieceGroup -> Bool) -> (PieceGroup -> PieceGroup) -> DB -> DB
modifyBy test action db =
    let
        toMap x =
            if test x then
                action x

            else
                x
    in
    makeDb (List.map toMap (all db))


map : (PieceGroup -> PieceGroup) -> DB -> DB
map action db =
    makeDb (List.map action (all db))


insert : PieceGroup -> DB -> DB
insert pg =
    KDDict.insert (makeKey pg) pg


findBy : (PieceGroup -> Bool) -> DB -> List PieceGroup
findBy filter db =
    List.filter filter (all db)


snap : Float -> DB -> DB
snap snapDistance db =
    case db |> getSelected of
        pg :: [] ->
            let
                memberIds =
                    Set.fromList <| List.map .id pg.members

                shouldBeMerged other =
                    (other.id == pg.id)
                        || (Set.intersect memberIds other.neighbours
                                |> Set.isEmpty
                                |> not
                           )

                dimensions =
                    let
                        { x, y } =
                            pg.position

                        radius =
                            floor snapDistance
                    in
                    { x = x - radius
                    , w = radius * 2
                    , y = y - radius
                    , h = radius * 2
                    }

                targets =
                    db
                        |> KDDict.findMatching
                            (matchWithin dimensions)
                        |> List.filter shouldBeMerged

                merge list =
                    case list of
                        [] ->
                            Nothing

                        head :: tail ->
                            Just <| List.foldl PieceGroup.merge head tail
            in
            case merge targets of
                Nothing ->
                    db

                Just merged ->
                    db
                        |> KDDict.removeAll (List.map makeKey targets)
                        |> insert merged

        _ ->
            db


boxSelect : Set.Set Int -> UI.SelectionMode -> Drag.Drag -> DB -> DB
boxSelect visibleGroups mode drag db =
    let
        targets =
            db
                |> KDDict.findMatching (matchBox <| Drag.getDimensions drag)
                |> List.filter (\pg -> Set.member pg.visibilityGroup visibleGroups)
    in
    case mode of
        UI.Add ->
            db
                |> KDDict.removeAll (List.map makeKey targets)
                |> KDDict.insertAllBy makeKey (List.map PieceGroup.select targets)

        UI.Remove ->
            db
                |> KDDict.removeAll (List.map makeKey targets)
                |> KDDict.insertAllBy makeKey (List.map PieceGroup.deselect targets)

        UI.Replace ->
            db
                |> KDDict.removeAll (List.map makeKey targets)
                |> map PieceGroup.deselect
                |> KDDict.insertAllBy makeKey (List.map PieceGroup.select targets)


clickedPieceGroup : Set.Set Int -> JigsawImage -> DB -> Point.Point -> Maybe PieceGroup
clickedPieceGroup visibleGroups_ _ db_ point =
    db_
        |> KDDict.findMatching
            (matchPoint point)
        |> List.filter (\pg -> Set.member pg.visibilityGroup visibleGroups_)
        |> order
        |> List.reverse
        |> List.head
