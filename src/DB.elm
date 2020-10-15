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

import Drag
import Group exposing (Group)
import KD.Match exposing (Match(..))
import KDDict exposing (KDDict, MatchKey)
import Point exposing (Point)
import Set exposing (Set)
import UI


type alias DbIndex =
    KDDict Int Group


type alias DB =
    { index : DbIndex
    , selected : Set.Set Group.ID
    , visibleGroups : Set.Set Int
    }


type alias DbKey =
    KDDict.Key Int


size : DB -> Int
size =
    .index >> KDDict.size


height : DB -> Int
height =
    .index >> KDDict.height


optimalHeight : DB -> Int
optimalHeight db =
    db
        |> size
        |> toFloat
        |> logBase 2
        |> floor


makeKey : Group -> DbKey
makeKey { position, minOffset, maxOffset } =
    let
        position_ =
            position
                |> Point.toPair

        x =
            ( position.x + minOffset.x, position.x + maxOffset.x )

        y =
            ( position.y + minOffset.y, position.y + maxOffset.y )
    in
    KDDict.coordinateKey position_
        |> KDDict.addCoordinateAxis x
        |> KDDict.addCoordinateAxis y


matchWithin : { x : Int, y : Int, w : Int, h : Int } -> MatchKey Int
matchWithin { x, y, w, h } =
    KDDict.coordinateKey
        ( WithinRange x (x + w), WithinRange y (y + h) )
        |> KDDict.addCoordinateAxis ( Anything, Anything )
        |> KDDict.addCoordinateAxis ( Anything, Anything )


matchBox : { x : Int, y : Int, w : Int, h : Int } -> MatchKey Int
matchBox { x, y, w, h } =
    KDDict.coordinateKey ( Anything, Anything )
        |> KDDict.addCoordinateAxis ( LargerThan x, SmallerThan (x + w) )
        |> KDDict.addCoordinateAxis ( LargerThan y, SmallerThan (y + h) )


matchPoint : Point -> MatchKey Int
matchPoint { x, y } =
    let
        radius =
            1
    in
    KDDict.coordinateKey ( Anything, Anything )
        |> KDDict.addCoordinateAxis
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
    { index = makeIndex pieceGroups
    , selected = Set.empty
    , visibleGroups = Set.fromList [ -1 ]
    }


makeIndex : List Group -> DbIndex
makeIndex list =
    KDDict.fromListBy makeKey list


getSelected : DB -> List Group
getSelected db =
    db.index
        |> KDDict.toList
        |> List.map Tuple.second
        |> List.filter (\group -> Set.member group.id db.selected)


getVisibleUnSelected : DB -> List Group
getVisibleUnSelected db =
    db.index
        |> KDDict.toList
        |> List.map Tuple.second
        |> List.filter (\group -> Set.member group.visibilityGroup db.visibleGroups)
        |> List.filter (\group -> not <| Set.member group.id db.selected)


modifySelected : (Group -> Group) -> DB -> DB
modifySelected f db =
    { db
        | index =
            db.index
                |> KDDict.toList
                |> List.map Tuple.second
                |> List.map
                    (\group ->
                        if Set.member group.id db.selected then
                            f group

                        else
                            group
                    )
                |> makeIndex
    }


insert : Group -> DbIndex -> DbIndex
insert pg =
    KDDict.insert (makeKey pg) pg


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
                    db.index
                        |> KDDict.findMatching
                            (matchWithin dimensions)
                        |> List.filter shouldBeMerged

                merge list =
                    List.foldl Group.merge pg list

                merged =
                    merge targets
            in
            { db
                | index =
                    db.index
                        |> KDDict.removeAll (List.map makeKey targets)
                        |> KDDict.insert (makeKey merged) merged
                , selected = Set.empty
            }

        _ ->
            if countDeleted db > size db then
                { db
                    | index =
                        db.index
                            |> KDDict.toList
                            |> KDDict.fromList
                }

            else
                db


boxSelect : UI.SelectionMode -> Drag.Drag -> DB -> DB
boxSelect mode drag db =
    let
        targets index =
            index
                |> KDDict.findMatching (matchBox <| Drag.getDimensions drag)
                |> List.filter (\pg -> Set.member pg.visibilityGroup db.visibleGroups)

        insideBox =
            targets db.index
                |> List.map .id
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
    db.index
        |> KDDict.findMatching
            (matchPoint point)
        |> List.filter (\pg -> Set.member pg.visibilityGroup db.visibleGroups)
        |> List.filter (Group.isPointInsideGroup point)
        |> List.reverse
        |> List.head


clickedAny : Point.Point -> DB -> Bool
clickedAny point db =
    db.index
        |> KDDict.findMatching
            (matchPoint point)
        |> List.filter (\group -> Set.member group.visibilityGroup db.visibleGroups)
        |> List.any (Group.isPointInsideGroup point)


clickedSelected : Point.Point -> DB -> Bool
clickedSelected point db =
    db.index
        |> KDDict.findMatching
            (matchPoint point)
        |> List.filter (\group -> Set.member group.id db.selected)
        |> List.filter (\group -> Set.member group.visibilityGroup db.visibleGroups)
        |> List.any (Group.isPointInsideGroup point)


countDeleted : DB -> Int
countDeleted =
    .index >> KDDict.countDeleted


heightDifference : DB -> Int
heightDifference =
    .index >> KDDict.heightDifference


getPieces : DB -> List ( Group.ID, Point )
getPieces db =
    let
        pgToP : Group -> List ( Group.ID, Point )
        pgToP pg =
            pg.members
                |> List.map (\p -> ( p.id, Point.add p.offset pg.position ))
    in
    KDDict.toList db.index
        |> List.map Tuple.second
        |> List.concatMap pgToP


sendSelectedToVisibilityGroup : Int -> DB -> DB
sendSelectedToVisibilityGroup x db =
    modifySelected (\pg -> { pg | visibilityGroup = x }) db


move : Int -> Drag.Drag -> DB -> DB
move radius drag db =
    db
        |> modifySelected (Group.move (Drag.distance drag))
        |> snap radius
