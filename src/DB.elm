module DB exposing
    ( DB
    , aggregateBy
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
import Maybe exposing (withDefault)
import PieceGroup exposing (PieceGroup)
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
makeKey { id, isSelected } =
    KDDict.key (keyBool isSelected)
        |> KDDict.addAxis (Tuple.second id)
        |> KDDict.addAxis (Tuple.first id)


type alias QueryConst =
    { id : Maybe ( Int, Int )
    , isSelected : Maybe Bool
    }


makeQuery : QueryConst -> DbQuery
makeQuery { id, isSelected } =
    KDDict.key (Maybe.map keyBool isSelected)
        |> KDDict.addAxis (Maybe.map Tuple.second id)
        |> KDDict.addAxis (Maybe.map Tuple.first id)


makeDb : List PieceGroup -> DB
makeDb list =
    KDDict.fromListBy makeKey list


getSelected : DB -> List PieceGroup
getSelected db =
    db
        |> KDDict.findAll
            (makeQuery
                { isSelected = Just True
                , id = Nothing
                }
            )


getUnSelected : DB -> List PieceGroup
getUnSelected db =
    db
        |> KDDict.findAll
            (makeQuery
                { isSelected = Just False
                , id = Nothing
                }
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
            in
            db
                |> aggregateBy
                    shouldBeMerged_
                    PieceGroup.merge

        _ ->
            db


aggregateBy : (PieceGroup -> Bool) -> (PieceGroup -> PieceGroup -> PieceGroup) -> DB -> DB
aggregateBy test combine db =
    let
        targets =
            all db
                |> List.filter test

        merge list =
            case list of
                [] ->
                    Nothing

                head :: tail ->
                    Just <| List.foldl combine head tail
    in
    case merge targets of
        Nothing ->
            db

        Just merged ->
            db
                |> KDDict.removeAll (List.map makeKey targets)
                |> insert merged
