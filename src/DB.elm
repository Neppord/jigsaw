module DB exposing
    ( DB
    , boxSelect
    , clickedPieceGroup
    , findBy
    , getSelected
    , getUnSelected
    , makeDb
    , map
    , modify
    , modifyBy
    , modifySelected
    , snap
    )

import Drag
import JigsawImage exposing (JigsawImage)
import KD.Match exposing (Match(..))
import KDDict exposing (KDDict, MatchKey)
import PieceGroup exposing (PieceGroup)
import Point
import Set
import UI


type alias DB =
    KDDict Int PieceGroup


type alias DbKey =
    KDDict.Key Int


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

        ( x1, x2 ) =
            ( position.x + minOffset.x, position.x + maxOffset.x )

        ( y1, y2 ) =
            ( position.y + minOffset.y, position.y + maxOffset.y )
    in
    KDDict.key (keyBool isSelected)
        |> KDDict.addAxis (Tuple.second id)
        |> KDDict.addAxis (Tuple.first id)
        |> KDDict.addAxis (Tuple.second position_)
        |> KDDict.addAxis (Tuple.first position_)
        |> KDDict.addAxis x2
        |> KDDict.addAxis x1
        |> KDDict.addAxis y2
        |> KDDict.addAxis y1


matchWithin : { x : Int, y : Int, w : Int, h : Int } -> MatchKey Int
matchWithin { x, y, w, h } =
    KDDict.key Anything
        |> KDDict.addAxis Anything
        |> KDDict.addAxis Anything
        |> KDDict.addAxis (WithinRange y (y + h))
        |> KDDict.addAxis (WithinRange x (x + w))
        |> KDDict.addAxis Anything
        |> KDDict.addAxis Anything
        |> KDDict.addAxis Anything
        |> KDDict.addAxis Anything


matchBox : { x : Int, y : Int, w : Int, h : Int } -> MatchKey Int
matchBox { x, y, w, h } =
    KDDict.key Anything
        |> KDDict.addAxis Anything
        |> KDDict.addAxis Anything
        |> KDDict.addAxis Anything
        |> KDDict.addAxis Anything
        {- max x -} |> KDDict.addAxis (SmallerThan (x + w))
        {- min x -} |> KDDict.addAxis (LargerThan x)
        {- max y -} |> KDDict.addAxis (SmallerThan (y + h))
        {- min y -} |> KDDict.addAxis (LargerThan y)


makeDb : List PieceGroup -> DB
makeDb list =
    KDDict.fromListBy makeKey list


getSelected : DB -> List PieceGroup
getSelected db =
    db
        |> KDDict.findMatching
            (KDDict.key (EqualTo (keyBool True))
                |> KDDict.addAxis Anything
                |> KDDict.addAxis Anything
                |> KDDict.addAxis Anything
                |> KDDict.addAxis Anything
                |> KDDict.addAxis Anything
                |> KDDict.addAxis Anything
                |> KDDict.addAxis Anything
                |> KDDict.addAxis Anything
            )
        |> List.sortBy .id


getUnSelected : DB -> List PieceGroup
getUnSelected db =
    db
        |> KDDict.findMatching
            (KDDict.key (EqualTo (keyBool False))
                |> KDDict.addAxis Anything
                |> KDDict.addAxis Anything
                |> KDDict.addAxis Anything
                |> KDDict.addAxis Anything
                |> KDDict.addAxis Anything
                |> KDDict.addAxis Anything
                |> KDDict.addAxis Anything
                |> KDDict.addAxis Anything
            )
        |> List.sortBy .id


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
clickedPieceGroup visibleGroups_ image_ db_ coordinate_ =
    db_
        |> findBy
            (PieceGroup.isPointInsidePieceGroup visibleGroups_ image_ coordinate_)
        |> List.reverse
        |> List.head
