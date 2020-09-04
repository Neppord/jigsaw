module DB exposing
    ( DB
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

import KDDict exposing (KDDict)
import Maybe
import PieceGroup exposing (PieceGroup)
import Point
import Set


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


emptyQuery : QueryConst
emptyQuery =
    { id = Nothing
    , isSelected = Nothing
    , position = Nothing
    }


makeQuery : QueryConst -> DbQuery
makeQuery { id, isSelected, position } =
    KDDict.key (Maybe.map keyBool isSelected)
        |> KDDict.addAxis (Maybe.map Tuple.second id)
        |> KDDict.addAxis (Maybe.map Tuple.first id)
        |> KDDict.addAxis (Maybe.map Tuple.second position)
        |> KDDict.addAxis (Maybe.map Tuple.first position)


makeDb : List PieceGroup -> DB
makeDb list =
    KDDict.fromListBy makeKey list


getSelected : DB -> List PieceGroup
getSelected db =
    db
        |> KDDict.findAll
            (makeQuery { emptyQuery | isSelected = Just True })


getUnSelected : DB -> List PieceGroup
getUnSelected db =
    db
        |> KDDict.findAll
            (makeQuery { emptyQuery | isSelected = Just False })


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


shouldBeMerged : Float -> PieceGroup -> PieceGroup -> Bool
shouldBeMerged snapDistance one other =
    let
        otherIds =
            Set.fromList <| List.map .id other.members
    in
    (PieceGroup.distance other one < snapDistance)
        && ((Set.size <| Set.intersect otherIds one.neighbours) > 0)


snap : Float -> DB -> DB
snap snapDistance db =
    case db |> getSelected of
        pg :: [] ->
            let
                shouldBeMerged_ x =
                    (x.id == pg.id)
                        || shouldBeMerged
                            snapDistance
                            pg
                            x

                position =
                    pg.position

                radius =
                    floor snapDistance

                topLeft =
                    position
                        |> Point.add (Point.Point -radius -radius)
                        |> Point.toPair

                bottomRight =
                    position
                        |> Point.add (Point.Point radius radius)
                        |> Point.toPair

                targets =
                    db
                        |> KDDict.findAllInRange
                            (KDDict.makeRangeQuery
                                (makeQuery { emptyQuery | position = Just topLeft })
                                (makeQuery { emptyQuery | position = Just bottomRight })
                            )
                        |> List.filter shouldBeMerged_

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
