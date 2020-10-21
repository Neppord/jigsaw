module DB exposing
    ( DB
    , boxSelect
    , clickedAny
    , clickedSelected
    , countDeleted
    , getPieces
    , getSelected
    , getVisibleUnSelected
    , height
    , heightDifference
    , makeDb
    , modifySelected
    , move
    , optimalHeight
    , selectAt
    , sendSelectedToVisibilityGroup
    , size
    , snap
    , toggleVisibilityGroup
    )

import Dict exposing (Dict)
import Drag
import Group exposing (Group)
import KD.Match exposing (Match(..))
import KDDict exposing (KDDict, MatchKey)
import Maybe.Extra
import Point exposing (Point)
import Set exposing (Set)
import UI


type alias DbIndex =
    KDDict Int Group.ID


type alias DB =
    { boxIndex : DbIndex
    , withinIndex : DbIndex
    , selected : Set.Set Group.ID
    , visibleGroups : Set.Set Int
    , groups : Dict Group.ID Group
    }


type alias DbKey =
    KDDict.Key Int


size : DB -> Int
size =
    .boxIndex >> KDDict.size


height : DB -> Int
height =
    .boxIndex >> KDDict.height


optimalHeight : DB -> Int
optimalHeight db =
    db
        |> size
        |> toFloat
        |> logBase 2
        |> floor


makeBoxKey : Group -> DbKey
makeBoxKey { position, minOffset, maxOffset } =
    let
        x =
            ( position.x + minOffset.x, position.x + maxOffset.x )

        y =
            ( position.y + minOffset.y, position.y + maxOffset.y )
    in
    KDDict.coordinateKey x
        |> KDDict.addCoordinateAxis y


makeWithinKey : Group -> DbKey
makeWithinKey { position } =
    let
        position_ =
            position
                |> Point.toPair
    in
    KDDict.coordinateKey position_


matchWithin : { x : Int, y : Int, w : Int, h : Int } -> MatchKey Int
matchWithin { x, y, w, h } =
    KDDict.coordinateKey
        ( WithinRange x (x + w), WithinRange y (y + h) )


matchBox : { x : Int, y : Int, w : Int, h : Int } -> MatchKey Int
matchBox { x, y, w, h } =
    KDDict.coordinateKey ( LargerThan x, SmallerThan (x + w) )
        |> KDDict.addCoordinateAxis ( LargerThan y, SmallerThan (y + h) )


matchPoint : Point -> MatchKey Int
matchPoint { x, y } =
    let
        radius =
            1
    in
    KDDict.coordinateKey
        ( SmallerThan (x + radius), LargerThan (x - radius) )
        |> KDDict.addCoordinateAxis
            ( SmallerThan (y + radius), LargerThan (y - radius) )



{- this could be replaced from Set.Extra -}


sToggle : comparable -> Set comparable -> Set comparable
sToggle a set =
    if Set.member a set then
        Set.remove a set

    else
        Set.insert a set


toggleVisibilityGroup : Int -> DB -> DB
toggleVisibilityGroup groupId db =
    { db
        | visibleGroups = sToggle groupId db.visibleGroups
    }


makeDb : List Group -> DB
makeDb pieceGroups =
    { boxIndex = makeBoxIndex pieceGroups
    , withinIndex = makeWithinIndex pieceGroups
    , selected = Set.empty
    , visibleGroups = Set.fromList [ -1 ]
    , groups =
        pieceGroups
            |> List.map (\g -> ( g.id, g ))
            |> Dict.fromList
    }


makeBoxIndex : List Group -> DbIndex
makeBoxIndex list =
    list
        |> List.map (\g -> ( makeBoxKey g, g.id ))
        |> KDDict.fromList


makeWithinIndex : List Group -> DbIndex
makeWithinIndex list =
    list
        |> List.map (\g -> ( makeWithinKey g, g.id ))
        |> KDDict.fromList


getSelected : DB -> List Group
getSelected db =
    db.selected
        |> Set.toList
        |> List.map (\id -> Dict.get id db.groups)
        |> Maybe.Extra.values


getVisibleUnSelected : DB -> List Group
getVisibleUnSelected db =
    db.groups
        |> Dict.values
        |> List.filter (\group -> Set.member group.visibilityGroup db.visibleGroups)
        |> List.filter (\group -> not <| Set.member group.id db.selected)


modifySelected : (Group -> Group) -> DB -> DB
modifySelected f db =
    let
        groups : Dict Group.ID Group
        groups =
            db.selected
                |> Set.foldl
                    (\id dict -> Dict.update id (Maybe.map f) dict)
                    db.groups
    in
    { db
        | boxIndex =
            groups
                |> Dict.values
                |> makeBoxIndex
        , withinIndex =
            groups
                |> Dict.values
                |> makeWithinIndex
        , groups = groups
    }


insert : Group -> DbIndex -> DbIndex
insert pg =
    KDDict.insert (makeBoxKey pg) pg.id


snap : Int -> DB -> DB
snap radius db =
    case db |> getSelected of
        pg :: [] ->
            let
                memberIds =
                    Set.fromList <| List.map .id pg.members

                shouldBeMerged other =
                    Set.member other.visibilityGroup db.visibleGroups
                        && (Set.intersect memberIds other.neighbours
                                |> Set.isEmpty
                                |> not
                           )
                        && (other.id /= pg.id)

                dimensions =
                    let
                        { x, y } =
                            pg.position
                    in
                    { x = x - radius
                    , w = radius * 2
                    , y = y - radius
                    , h = radius * 2
                    }

                targets =
                    db.withinIndex
                        |> KDDict.findMatching
                            (matchWithin dimensions)
                        |> List.map (\id -> Dict.get id db.groups)
                        |> Maybe.Extra.values
                        |> List.filter shouldBeMerged

                merge list =
                    List.foldl Group.merge pg list

                merged =
                    merge targets
            in
            { db
                | boxIndex =
                    db.boxIndex
                        |> KDDict.removeAll (List.map makeBoxKey (pg :: targets))
                        |> KDDict.insert (makeBoxKey merged) merged.id
                , withinIndex =
                    db.withinIndex
                        |> KDDict.removeAll (List.map makeWithinKey (pg :: targets))
                        |> KDDict.insert (makeWithinKey merged) merged.id
                , groups =
                    (pg :: targets)
                        |> List.map .id
                        |> List.foldl (\id dict -> Dict.remove id dict) db.groups
                        |> Dict.insert merged.id merged
                , selected = Set.singleton merged.id
            }

        _ ->
            if countDeleted db > size db then
                { db
                    | boxIndex =
                        db.boxIndex
                            |> KDDict.toList
                            |> KDDict.fromList
                    , withinIndex =
                        db.withinIndex
                            |> KDDict.toList
                            |> KDDict.fromList
                }

            else
                db


boxSelect : UI.SelectionMode -> Drag.Drag -> DB -> DB
boxSelect mode drag db =
    let
        insideBox =
            db.boxIndex
                |> KDDict.findMatching (matchBox <| Drag.getDimensions drag)
                |> List.filter
                    (\id ->
                        Dict.get id db.groups
                            |> Maybe.map (\pg -> Set.member pg.visibilityGroup db.visibleGroups)
                            |> Maybe.withDefault False
                    )
                |> Set.fromList
    in
    case mode of
        UI.Add ->
            { db | selected = db.selected |> Set.union insideBox }

        UI.Remove ->
            { db | selected = db.selected |> Set.diff insideBox }

        UI.Replace ->
            { db | selected = insideBox }


selectAt : Point.Point -> UI.SelectionMode -> DB -> DB
selectAt point mode db =
    if clickedSelected point db then
        db

    else
        case clickedUnselectedPieceGroup point db of
            Nothing ->
                db

            Just pg ->
                db |> select mode pg


select : UI.SelectionMode -> Group -> DB -> DB
select mode pg db =
    case mode of
        UI.Add ->
            { db
                | selected =
                    db.selected
                        |> Set.insert pg.id
            }

        UI.Remove ->
            { db
                | selected =
                    db.selected
                        |> Set.remove pg.id
            }

        UI.Replace ->
            { db
                | selected =
                    Set.singleton pg.id
            }


clickedUnselectedPieceGroup : Point.Point -> DB -> Maybe Group
clickedUnselectedPieceGroup point db =
    db.boxIndex
        |> KDDict.findMatching
            (matchPoint point)
        |> List.map (\id -> Dict.get id db.groups)
        |> Maybe.Extra.values
        |> List.filter (\pg -> Set.member pg.visibilityGroup db.visibleGroups)
        |> List.filter (Group.isPointInsideGroup point)
        |> List.reverse
        |> List.head


clickedAny : Point.Point -> DB -> Bool
clickedAny point db =
    db.boxIndex
        |> KDDict.findMatching
            (matchPoint point)
        |> List.map (\id -> Dict.get id db.groups)
        |> Maybe.Extra.values
        |> List.filter (\group -> Set.member group.visibilityGroup db.visibleGroups)
        |> List.any (Group.isPointInsideGroup point)


clickedSelected : Point.Point -> DB -> Bool
clickedSelected point db =
    db.boxIndex
        |> KDDict.findMatching
            (matchPoint point)
        |> List.map (\id -> Dict.get id db.groups)
        |> Maybe.Extra.values
        |> List.filter (\group -> Set.member group.id db.selected)
        |> List.filter (\group -> Set.member group.visibilityGroup db.visibleGroups)
        |> List.any (Group.isPointInsideGroup point)


countDeleted : DB -> Int
countDeleted =
    .boxIndex >> KDDict.countDeleted


heightDifference : DB -> Int
heightDifference =
    .boxIndex >> KDDict.heightDifference


getPieces : DB -> List ( Group.ID, Point )
getPieces db =
    let
        pgToP : Group -> List ( Group.ID, Point )
        pgToP pg =
            pg.members
                |> List.map (\p -> ( p.id, Point.add p.offset pg.position ))
    in
    db.groups
        |> Dict.values
        |> List.concatMap pgToP


sendSelectedToVisibilityGroup : Int -> DB -> DB
sendSelectedToVisibilityGroup x db =
    modifySelected (\pg -> { pg | visibilityGroup = x }) db


move : Int -> Drag.Drag -> DB -> DB
move radius drag db =
    db
        |> modifySelected (Group.move (Drag.distance drag))
        |> snap radius
