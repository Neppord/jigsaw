module DB exposing
    ( DB, findBy
    , aggregateBy
    , getSelected
    , getUnSelected
    , makeDb
    , map
    , modify
    , modifyBy
    , modifySelected
    )
import Maybe exposing (withDefault)
import PieceGroup exposing (PieceGroup)


type DB
    = DB
        { selected : List PieceGroup
        , unSelected : List PieceGroup
        }


makeDb : List PieceGroup -> DB
makeDb list =
    let
        ( selected, unSelected ) =
            list
                |> List.partition .isSelected
    in
    DB
        { selected = selected
        , unSelected = unSelected
        }


getSelected : DB -> List PieceGroup
getSelected (DB { selected }) =
    selected


getUnSelected : DB -> List PieceGroup
getUnSelected (DB { unSelected }) =
    unSelected


modifySelected : (PieceGroup -> PieceGroup) -> DB -> DB
modifySelected f (DB { selected, unSelected }) =
    let
        ( s, u ) =
            selected
                |> List.map f
                |> List.partition .isSelected
    in
    DB
        { selected = s
        , unSelected = u ++ unSelected
        }


modify : PieceGroup.ID -> (PieceGroup -> PieceGroup) -> DB -> DB
modify id action (DB { selected, unSelected }) =
    let
        ( s, u ) =
            selected
                ++ unSelected
                |> List.map
                    (\x ->
                        if x.id == id then
                            action x

                        else
                            x
                    )
                |> List.partition .isSelected
    in
    DB { selected = s, unSelected = u }


modifyBy : (PieceGroup -> Bool) -> (PieceGroup -> PieceGroup) -> DB -> DB
modifyBy test action (DB { selected, unSelected }) =
    let
        ( s, u ) =
            selected
                ++ unSelected
                |> List.map
                    (\x ->
                        if test x then
                            action x

                        else
                            x
                    )
                |> List.partition .isSelected
    in
    DB { selected = s, unSelected = u }


map : (PieceGroup -> PieceGroup) -> DB -> DB
map action (DB { selected, unSelected }) =
    let
        ( s, u ) =
            selected
                ++ unSelected
                |> List.map action
                |> List.partition .isSelected
    in
    DB { selected = s, unSelected = u }


aggregateBy : (PieceGroup -> Bool) -> (PieceGroup -> PieceGroup -> PieceGroup) -> DB -> DB
aggregateBy test combine (DB { selected, unSelected }) =
    let
        ( s, u ) =
            selected
                ++ unSelected
                |> List.partition test
                |> Tuple.mapFirst merge
                |> (\( maybe, rest ) ->
                        Just rest
                            |> Maybe.map2 (::) maybe
                            |> Maybe.withDefault rest
                   )
                |> List.partition .isSelected

        merge list =
            case list of
                [] ->
                    Nothing

                head :: tail ->
                    Just <| List.foldl combine head tail
    in
    DB { selected = s, unSelected = u }

findBy : (PieceGroup -> Bool) -> DB -> List PieceGroup
findBy filter (DB {selected, unSelected}) =
    List.filter filter unSelected ++ List.filter filter selected