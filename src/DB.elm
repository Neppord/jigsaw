module DB exposing
    ( DB
    , boxSelect
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
import KD.Match exposing (Match(..))
import KDDict exposing (KDDict, MatchKey)
import Maybe
import PieceGroup exposing (PieceGroup)
import Point
import Set
import UI


type alias DB =
    KDDict Int PieceGroup


type alias DbKey =
    KDDict.Key Int


type alias DbQuery =
    KDDict.Key (Maybe Int)


keyBool : Bool -> Int
keyBool v =
    if v then
        1

    else
        0


makeKey : PieceGroup -> DbKey
makeKey { id, isSelected, position } =
    let
        position_ =
            position
                |> Point.toPair
    in
    KDDict.key (keyBool isSelected)
        |> KDDict.addAxis (Tuple.second id)
        |> KDDict.addAxis (Tuple.first id)
        |> KDDict.addAxis (Tuple.second position_)
        |> KDDict.addAxis (Tuple.first position_)


type alias QueryConst =
    { id : Maybe ( Int, Int )
    , isSelected : Maybe Bool
    , position : Maybe ( Int, Int )
    }


matchWithin : { x : Int, y : Int, w : Int, h : Int } -> MatchKey Int
matchWithin { x, y, w, h } =
    KDDict.key Anything
        |> KDDict.addAxis Anything
        |> KDDict.addAxis Anything
        |> KDDict.addAxis (WithinRange y (y + h))
        |> KDDict.addAxis (WithinRange x (x + w))


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
            )


getUnSelected : DB -> List PieceGroup
getUnSelected db =
    db
        |> KDDict.findMatching
            (KDDict.key (EqualTo (keyBool False))
                |> KDDict.addAxis Anything
                |> KDDict.addAxis Anything
                |> KDDict.addAxis Anything
                |> KDDict.addAxis Anything
            )


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
        { x, y, w, h } =
            Drag.getDimensions drag

        topLeft =
            Point.Point x y

        bottomRight =
            Point.Point (x + w) (y + h)

        isWithin =
            PieceGroup.isPieceGroupInsideBox topLeft bottomRight

        shouldBeSelected pg =
            Set.member pg.visibilityGroup visibleGroups
                && isWithin pg
    in
    case mode of
        UI.Add ->
            db
                |> modifyBy
                    (\pg -> shouldBeSelected pg || pg.isSelected)
                    PieceGroup.select

        UI.Remove ->
            db
                |> modifyBy
                    (\pg -> shouldBeSelected pg || not pg.isSelected)
                    PieceGroup.deselect

        UI.Replace ->
            db
                |> map
                    (\pg ->
                        if pg |> shouldBeSelected then
                            PieceGroup.select pg

                        else
                            PieceGroup.deselect pg
                    )
