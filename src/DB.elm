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
import KD.Match exposing (Match(..))
import KDDict exposing (KDDict, MatchKey)
import PieceGroup exposing (Group)
import Point exposing (Point)
import Set exposing (Set)
import UI


type alias DbIndex =
    KDDict Int Group


type alias GroupID =
    Int


type alias DB =
    { unselected : DbIndex
    , selected : DbIndex
    , visibleGroups : Set.Set Int
    }


type alias DbKey =
    KDDict.Key Int


size : DB -> Int
size =
    .unselected >> KDDict.size


height : DB -> Int
height =
    .unselected >> KDDict.height


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
    { unselected = makeIndex pieceGroups
    , selected = KDDict.fromList []
    , visibleGroups = Set.fromList [ -1 ]
    }


makeIndex : List Group -> DbIndex
makeIndex list =
    KDDict.fromListBy makeKey list


getSelected : DB -> List Group
getSelected db =
    db.selected
        |> KDDict.toList
        |> List.map Tuple.second


getVisibleUnSelected : DB -> List Group
getVisibleUnSelected db =
    db.unselected
        |> KDDict.toList
        |> List.map Tuple.second
        |> List.filter (\pg -> Set.member pg.visibilityGroup db.visibleGroups)


modifySelected : (Group -> Group) -> DB -> DB
modifySelected f db =
    { db
        | selected =
            db.selected
                |> KDDict.toList
                |> List.map Tuple.second
                |> List.map f
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
                    db.unselected
                        |> KDDict.findMatching
                            (matchWithin dimensions)
                        |> List.filter shouldBeMerged

                merge list =
                    List.foldl PieceGroup.merge pg list

                merged =
                    merge targets
            in
            { db
                | unselected =
                    db.unselected
                        |> KDDict.removeAll (List.map makeKey targets)
                , selected = makeIndex [ merged ]
            }

        _ ->
            if countDeleted db > size db then
                { db
                    | unselected =
                        db.unselected
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
    in
    case mode of
        UI.Add ->
            let
                toMove =
                    targets db.unselected
            in
            { db
                | unselected =
                    db.unselected
                        |> (KDDict.removeAll << List.map makeKey) toMove
                , selected =
                    db.selected
                        |> KDDict.insertAllBy makeKey toMove
            }

        UI.Remove ->
            let
                toMove =
                    targets db.selected
            in
            { db
                | selected =
                    db.selected
                        |> (KDDict.removeAll << List.map makeKey) toMove
                , unselected =
                    db.unselected
                        |> KDDict.insertAllBy makeKey toMove
            }

        UI.Replace ->
            let
                all =
                    db.unselected
                        |> KDDict.insertAll (KDDict.toList db.selected)

                selected =
                    all
                        |> targets

                unselected =
                    all
                        |> KDDict.removeAll
                            (List.map makeKey selected)
            in
            { db
                | unselected = unselected
                , selected = makeIndex selected
            }


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
                | unselected =
                    db.unselected
                        |> KDDict.remove (makeKey pg)
                , selected =
                    db.selected
                        |> KDDict.insert (makeKey pg) pg
            }

        UI.Remove ->
            { db
                | unselected =
                    db.unselected
                        |> KDDict.insert (makeKey pg) pg
                , selected =
                    db.selected
                        |> KDDict.remove (makeKey pg)
            }

        UI.Replace ->
            { db
                | unselected =
                    db.unselected
                        |> KDDict.insertAll (KDDict.toList db.selected)
                        |> KDDict.remove (makeKey pg)
                , selected =
                    makeIndex [ pg ]
            }


clickedUnselectedPieceGroup : Point.Point -> DB -> Maybe Group
clickedUnselectedPieceGroup point db =
    db.unselected
        |> KDDict.findMatching
            (matchPoint point)
        |> List.filter (\pg -> Set.member pg.visibilityGroup db.visibleGroups)
        |> List.filter (PieceGroup.isPointInsideGroup point)
        |> List.reverse
        |> List.head


clickedUnselected : Point.Point -> DB -> Bool
clickedUnselected point db =
    db.unselected
        |> KDDict.findMatching
            (matchPoint point)
        |> List.filter (\pg -> Set.member pg.visibilityGroup db.visibleGroups)
        |> List.any (PieceGroup.isPointInsideGroup point)


clickedAny : Point.Point -> DB -> Bool
clickedAny point db =
    clickedSelected point db || clickedUnselected point db


clickedSelected : Point.Point -> DB -> Bool
clickedSelected point db =
    db.selected
        |> KDDict.findMatching
            (matchPoint point)
        |> List.filter (\pg -> Set.member pg.visibilityGroup db.visibleGroups)
        |> List.any (PieceGroup.isPointInsideGroup point)


countDeleted : DB -> Int
countDeleted =
    .unselected >> KDDict.countDeleted


heightDifference : DB -> Int
heightDifference =
    .unselected >> KDDict.heightDifference


getPieces : DB -> List ( PieceGroup.ID, Point )
getPieces db =
    let
        pgToP : Group -> List ( PieceGroup.ID, Point )
        pgToP pg =
            pg.members
                |> List.map (\p -> ( p.id, Point.add p.offset pg.position ))
    in
    KDDict.toList db.selected
        ++ KDDict.toList db.unselected
        |> List.map Tuple.second
        |> List.concatMap pgToP


sendSelectedToVisibilityGroup : Int -> DB -> DB
sendSelectedToVisibilityGroup x db =
    modifySelected (\pg -> { pg | visibilityGroup = x }) db


move : Int -> Drag.Drag -> DB -> DB
move radius drag db =
    db
        |> modifySelected (PieceGroup.move (Drag.distance drag))
        |> snap radius
