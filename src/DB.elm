module DB exposing
    ( DB
    , boxSelect
    , clickedPieceGroup
    , countDeleted
    , getSelected
    , getUnSelected
    , height
    , heightDifference
    , makeDb
    , modifySelected
    , optimalHeight
    , select
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


makeDb : List PieceGroup -> DB
makeDb list =
    KDDict.fromListBy makeKey list


getSelected : DB -> List PieceGroup
getSelected db =
    db
        |> all
        |> List.filter .isSelected


getUnSelected : DB -> List PieceGroup
getUnSelected db =
    db
        |> all
        |> List.filter (not << .isSelected)


modifySelected : (PieceGroup -> PieceGroup) -> DB -> DB
modifySelected f db =
    let
        selected =
            getSelected db

        modified =
            List.map f selected
    in
    db
        |> KDDict.removeAll (List.map makeKey selected)
        |> KDDict.insertAllBy makeKey modified


all : DB -> List PieceGroup
all db =
    db
        |> KDDict.toList
        |> List.map Tuple.second


insert : PieceGroup -> DB -> DB
insert pg =
    KDDict.insert (makeKey pg) pg


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
            if countDeleted db > size db then
                db |> all |> makeDb

            else
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
                |> KDDict.unsafeMap
                    (\pg ->
                        if List.member pg targets then
                            PieceGroup.select pg

                        else
                            pg
                    )

        UI.Remove ->
            db
                |> KDDict.unsafeMap
                    (\pg ->
                        if List.member pg targets then
                            PieceGroup.deselect pg

                        else
                            pg
                    )

        UI.Replace ->
            db
                |> KDDict.unsafeMap
                    (\pg ->
                        if List.member pg targets then
                            PieceGroup.select pg

                        else
                            PieceGroup.deselect pg
                    )


clickedPieceGroup : Set.Set Int -> JigsawImage -> DB -> Point.Point -> Maybe PieceGroup
clickedPieceGroup visibleGroups_ _ db_ point =
    db_
        |> KDDict.findMatching
            (matchPoint point)
        |> List.filter (\pg -> Set.member pg.visibilityGroup visibleGroups_)
        |> List.reverse
        |> List.head


select : UI.SelectionMode -> PieceGroup -> DB -> DB
select mode pg db =
    case mode of
        UI.Add ->
            db
                |> KDDict.unsafeMap
                    (\other ->
                        if other.id == pg.id then
                            PieceGroup.select other

                        else
                            other
                    )

        UI.Replace ->
            db
                |> KDDict.unsafeMap
                    (\other ->
                        if other.id == pg.id then
                            PieceGroup.select other

                        else
                            PieceGroup.deselect other
                    )

        UI.Remove ->
            db
                |> KDDict.unsafeMap
                    (\other ->
                        if other.id == pg.id then
                            PieceGroup.deselect other

                        else
                            other
                    )


countDeleted : DB -> Int
countDeleted =
    KDDict.countDeleted


heightDifference : DB -> Int
heightDifference =
    KDDict.heightDifference
