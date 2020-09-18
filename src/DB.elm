module DB exposing
    ( DB
    , boxSelect
    , clickedPieceGroup
    , countDeleted
    , getPieces
    , getSelected
    , getVisibleUnSelected
    , height
    , heightDifference
    , makeDb
    , makeIndex
    , modifySelected
    , optimalHeight
    , select
    , size
    , snap
    , toggleVisibilityGroup
    )

import Drag
import JigsawImage exposing (JigsawImage)
import KD.Match exposing (Match(..))
import KDDict exposing (KDDict, MatchKey)
import PieceGroup exposing (PieceGroup)
import Point exposing (Point)
import Set exposing (Set)
import UI


type alias DbIndex =
    KDDict Int PieceGroup


type alias DB =
    { index : DbIndex
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


makeKey : PieceGroup -> DbKey
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


makeDb : List PieceGroup -> DB
makeDb pieceGroups =
    { index = makeIndex pieceGroups
    , visibleGroups = Set.fromList [ -1 ]
    }


makeIndex : List PieceGroup -> DbIndex
makeIndex list =
    KDDict.fromListBy makeKey list


getSelected : DB -> List PieceGroup
getSelected db =
    db
        |> all
        |> List.filter .isSelected


getVisibleUnSelected : DB -> List PieceGroup
getVisibleUnSelected db =
    db
        |> all
        |> List.filter (not << .isSelected)
        |> List.filter (\pg -> Set.member pg.visibilityGroup db.visibleGroups)


modifySelected : (PieceGroup -> PieceGroup) -> DB -> DB
modifySelected f db =
    let
        selected =
            getSelected db

        modified =
            List.map f selected
    in
    { db
        | index =
            db.index
                |> KDDict.removeAll (List.map makeKey selected)
                |> KDDict.insertAllBy makeKey modified
    }


all : DB -> List PieceGroup
all db =
    db.index
        |> KDDict.toList
        |> List.map Tuple.second


insert : PieceGroup -> DbIndex -> DbIndex
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
                        && ((other.id == pg.id)
                                || (Set.intersect memberIds other.neighbours
                                        |> Set.isEmpty
                                        |> not
                                   )
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
                    { db
                        | index =
                            db.index
                                |> KDDict.removeAll (List.map makeKey targets)
                                |> insert merged
                    }

        _ ->
            if countDeleted db > size db then
                { db | index = db |> all |> makeIndex }

            else
                db


boxSelect : UI.SelectionMode -> Drag.Drag -> DB -> DB
boxSelect mode drag db =
    let
        targets =
            db.index
                |> KDDict.findMatching (matchBox <| Drag.getDimensions drag)
                |> List.filter (\pg -> Set.member pg.visibilityGroup db.visibleGroups)
    in
    { db
        | index =
            case mode of
                UI.Add ->
                    db.index
                        |> KDDict.unsafeMap
                            (\pg ->
                                if List.member pg targets then
                                    PieceGroup.select pg

                                else
                                    pg
                            )

                UI.Remove ->
                    db.index
                        |> KDDict.unsafeMap
                            (\pg ->
                                if List.member pg targets then
                                    PieceGroup.deselect pg

                                else
                                    pg
                            )

                UI.Replace ->
                    db.index
                        |> KDDict.unsafeMap
                            (\pg ->
                                if List.member pg targets then
                                    PieceGroup.select pg

                                else
                                    PieceGroup.deselect pg
                            )
    }


clickedPieceGroup : JigsawImage -> DB -> Point.Point -> Maybe PieceGroup
clickedPieceGroup _ db_ point =
    db_.index
        |> KDDict.findMatching
            (matchPoint point)
        |> List.filter (\pg -> Set.member pg.visibilityGroup db_.visibleGroups)
        |> List.filter (PieceGroup.isPointInsidePieceGroup point)
        |> List.reverse
        |> List.head


select : UI.SelectionMode -> PieceGroup -> DB -> DB
select mode pg db =
    { db
        | index =
            case mode of
                UI.Add ->
                    db.index
                        |> KDDict.unsafeMap
                            (\other ->
                                if other.id == pg.id then
                                    PieceGroup.select other

                                else
                                    other
                            )

                UI.Replace ->
                    db.index
                        |> KDDict.unsafeMap
                            (\other ->
                                if other.id == pg.id then
                                    PieceGroup.select other

                                else
                                    PieceGroup.deselect other
                            )

                UI.Remove ->
                    db.index
                        |> KDDict.unsafeMap
                            (\other ->
                                if other.id == pg.id then
                                    PieceGroup.deselect other

                                else
                                    other
                            )
    }


countDeleted : DB -> Int
countDeleted =
    .index >> KDDict.countDeleted


heightDifference : DB -> Int
heightDifference =
    .index >> KDDict.heightDifference


getPieces : DB -> List ( PieceGroup.ID, Point )
getPieces db =
    let
        pgToP : PieceGroup -> List ( PieceGroup.ID, Point )
        pgToP pg =
            pg.members
                |> List.map (\p -> ( p.id, Point.add p.offset pg.position ))
    in
    all db
        |> List.concatMap pgToP
