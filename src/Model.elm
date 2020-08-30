module Model exposing
    ( Box
    , Key(..)
    , Keyboard
    , Model
    , Msg(..)
    , NewModel(..)
    , OldModel
    , Selected(..)
    , SelectionBox(..)
    , boxBottomRight
    , boxTopLeft
    , defaultPieceGroup
    , generateNewModel
    , getEdges
    , getImage
    , getVisibilityGroups
    , init
    , toNewModel
    , toOldModel
    )

import Dict
import Drag exposing (Drag)
import Edge exposing (Edge, generateEdgePoints)
import JigsawImage
    exposing
        ( JigsawImage
        , createPieceGroups
        , isPieceGroupInsideBox
        , shufflePiecePositions
        )
import List
import PieceGroup exposing (PieceGroup)
import Point exposing (Point)
import Random
import Seeded exposing (Seeded(..))
import Set as S exposing (Set)


type Msg
    = MouseDown Point Keyboard
    | MouseMove Point
    | MouseUp
    | Scramble
    | KeyChanged Bool Key


type alias Keyboard =
    { shift : Bool
    , ctrl : Bool
    }


type alias Configuration =
    { snapDistance : Float
    , image : JigsawImage
    }


type alias Model =
    Seeded OldModel


type alias OldModel =
    InnerModel Configuration


type alias InnerModel a =
    { a
        | cursor : Maybe Point
        , pieceGroups : Dict.Dict Int PieceGroup
        , selected : Selected
        , selectionBox : SelectionBox
        , edges : List (List Edge)
        , visibleGroups : S.Set Int
        , keyboard : Keyboard
    }


toConfiguration : OldModel -> Configuration
toConfiguration oldModel =
    { image = oldModel.image
    , snapDistance = oldModel.snapDistance
    }


type NewModel
    = Identity
        { oldModel : OldModel
        , configuration : Configuration
        , selected : List PieceGroup
        , unSelected : List PieceGroup
        }
    | Moving
        { oldModel : OldModel
        , configuration : Configuration
        , drag : Drag
        , selected : List PieceGroup
        , unSelected : List PieceGroup
        }
    | SelectingWithBox
        { oldModel : OldModel
        , configuration : Configuration
        , drag : Drag
        , within : List PieceGroup
        , alreadySelected : List PieceGroup
        , unSelected : List PieceGroup
        }
    | DeselectingWithBox
        { oldModel : OldModel
        , configuration : Configuration
        , drag : Drag
        , within : List PieceGroup
        , alreadySelected : List PieceGroup
        , unSelected : List PieceGroup
        }


toNewModel : OldModel -> NewModel
toNewModel oldModel =
    let
        ( selected, unSelected ) =
            Dict.partition (always .isSelected) oldModel.pieceGroups
                |> Tuple.mapBoth Dict.values Dict.values
    in
    case ( oldModel.cursor, oldModel.selectionBox ) of
        ( Just current, NullBox ) ->
            Moving
                { oldModel = oldModel
                , configuration = toConfiguration oldModel
                , drag = Drag.from current
                , selected = selected
                , unSelected = unSelected
                }

        ( Just _, Normal box ) ->
            let
                within =
                    oldModel.pieceGroups
                        |> Dict.values
                        |> List.filter
                            (\pg -> S.member pg.visibilityGroup oldModel.visibleGroups)
                        |> List.filter
                            (isPieceGroupInsideBox
                                oldModel.image
                                (boxTopLeft box)
                                (boxBottomRight box)
                            )

                alreadySelected =
                    oldModel.pieceGroups
                        |> Dict.values
                        |> List.filter (\pg -> S.member pg.id box.selectedIds)

                notSelected =
                    oldModel.pieceGroups
                        |> Dict.values
                        |> List.filter (\pg -> not <| S.member pg.id box.selectedIds)
            in
            SelectingWithBox
                { oldModel = oldModel
                , configuration = toConfiguration oldModel
                , drag =
                    Drag.from box.staticCorner
                        |> Drag.to box.movingCorner
                , alreadySelected = alreadySelected
                , within = within
                , unSelected = notSelected
                }

        ( Just _, Inverted box ) ->
            let
                within =
                    oldModel.pieceGroups
                        |> Dict.values
                        |> List.filter
                            (\pg -> S.member pg.visibilityGroup oldModel.visibleGroups)
                        |> List.filter
                            (isPieceGroupInsideBox
                                oldModel.image
                                (boxTopLeft box)
                                (boxBottomRight box)
                            )

                alreadySelected =
                    oldModel.pieceGroups
                        |> Dict.values
                        |> List.filter (\pg -> S.member pg.id box.selectedIds)

                notSelected =
                    oldModel.pieceGroups
                        |> Dict.values
                        |> List.filter (\pg -> not <| S.member pg.id box.selectedIds)
            in
            DeselectingWithBox
                { oldModel = oldModel
                , configuration = toConfiguration oldModel
                , drag =
                    Drag.from box.staticCorner
                        |> Drag.to box.movingCorner
                , alreadySelected = alreadySelected
                , within = within
                , unSelected = notSelected
                }

        ( _, _ ) ->
            Identity
                { oldModel = oldModel
                , configuration = toConfiguration oldModel
                , selected = selected
                , unSelected = unSelected
                }


toOldModel : NewModel -> OldModel
toOldModel newModel =
    case newModel of
        Identity { oldModel } ->
            oldModel

        Moving { oldModel, drag, selected, unSelected } ->
            { oldModel
                | pieceGroups =
                    selected
                        |> List.map (PieceGroup.move (Drag.distance drag))
                        |> List.append unSelected
                        |> List.map (\pg -> ( pg.id, pg ))
                        |> Dict.fromList
                , cursor = Just <| Drag.getCurrent drag
            }

        SelectingWithBox { oldModel, drag, alreadySelected, within } ->
            let
                withinIds =
                    within
                        |> List.map .id
                        |> S.fromList

                alreadySelectedIds =
                    alreadySelected
                        |> List.map .id
                        |> S.fromList

                updatedPieceGroups =
                    oldModel.pieceGroups
                        |> Dict.map
                            (\id pg ->
                                { pg
                                    | isSelected =
                                        S.member id alreadySelectedIds
                                            || S.member id withinIds
                                }
                            )

                box =
                    { staticCorner = Drag.getStart drag
                    , movingCorner = Drag.getCurrent drag
                    , selectedIds = alreadySelectedIds
                    }
            in
            { oldModel
                | selectionBox = Normal box
                , pieceGroups = updatedPieceGroups
            }

        DeselectingWithBox { oldModel, drag, alreadySelected, within } ->
            let
                withinIds =
                    within
                        |> List.map .id
                        |> S.fromList

                alreadySelectedIds =
                    alreadySelected
                        |> List.map .id
                        |> S.fromList

                updatedPieceGroups =
                    oldModel.pieceGroups
                        |> Dict.map
                            (\id pg ->
                                { pg
                                    | isSelected =
                                        S.member id alreadySelectedIds
                                            && not (S.member id withinIds)
                                }
                            )

                box =
                    { staticCorner = Drag.getStart drag
                    , movingCorner = Drag.getCurrent drag
                    , selectedIds = alreadySelectedIds
                    }
            in
            { oldModel
                | selectionBox = Inverted box
                , pieceGroups = updatedPieceGroups
            }


init : () -> ( Seeded NewModel, Cmd Msg )
init () =
    let
        image =
            { path = "../resources/kitten.png"
            , width = 533
            , height = 538
            , xpieces = 4
            , ypieces = 4
            , scale = 1.0
            }
    in
    ( image
        |> Seeded (Random.initialSeed 0)
        |> Seeded.map generateNewModel
        |> Seeded.step
    , Cmd.none
    )


buildModel :
    JigsawImage
    -> Dict.Dict Int PieceGroup
    -> List (List Edge)
    -> OldModel
buildModel image pieceGroups edges =
    { cursor = Nothing
    , selected = NullSelection
    , snapDistance = 30.0
    , selectionBox = NullBox
    , visibleGroups = S.fromList [ -1 ]
    , keyboard = { shift = False, ctrl = False }
    , image = image
    , edges = edges
    , pieceGroups = pieceGroups
    }


generateModel : JigsawImage -> Random.Generator OldModel
generateModel image =
    let
        numberOfEdges =
            2 * image.xpieces * image.ypieces - image.xpieces - image.ypieces

        generatePositions =
            shufflePiecePositions image.width image.height image

        generatePieceGroups =
            generatePositions
                |> Random.map (createPieceGroups image)

        generateEdges =
            Random.map
                (\eps ->
                    List.range 0 (image.xpieces * image.ypieces - 1)
                        |> List.map (\id -> Edge.pieceEdges image.xpieces image.ypieces id eps)
                )
                (generateEdgePoints numberOfEdges)
    in
    Random.map2 (buildModel image) generatePieceGroups generateEdges


generateNewModel : JigsawImage -> Random.Generator NewModel
generateNewModel image =
    generateModel image
        |> Random.map toNewModel


type SelectionBox
    = Normal Box
    | Inverted Box
    | NullBox


type alias Box =
    { staticCorner : Point
    , movingCorner : Point
    , selectedIds : S.Set Int
    }


type Selected
    = Multiple
    | Single Int
    | NullSelection


type Key
    = Number Int
    | Control
    | Shift
    | Other


boxTopLeft : Box -> Point
boxTopLeft box =
    Point
        (min box.staticCorner.x box.movingCorner.x)
        (min box.staticCorner.y box.movingCorner.y)


boxBottomRight : Box -> Point
boxBottomRight box =
    Point
        (max box.staticCorner.x box.movingCorner.x)
        (max box.staticCorner.y box.movingCorner.y)



-- Until I figure out how to handle index out of bounds
-- exceptions more elegantly


defaultPieceGroup : PieceGroup
defaultPieceGroup =
    { position = Point 0 0
    , isSelected = False
    , id = -10
    , neighbours = S.empty
    , members = []
    , visibilityGroup = -1
    }


getVisibilityGroups : NewModel -> Set Int
getVisibilityGroups model =
    case model of
        Identity { oldModel } ->
            oldModel.visibleGroups

        Moving { oldModel } ->
            oldModel.visibleGroups

        SelectingWithBox { oldModel } ->
            oldModel.visibleGroups

        DeselectingWithBox { oldModel } ->
            oldModel.visibleGroups


getEdges : NewModel -> List (List Edge)
getEdges model =
    case model of
        Identity { oldModel } ->
            oldModel.edges

        Moving { oldModel } ->
            oldModel.edges

        SelectingWithBox { oldModel } ->
            oldModel.edges

        DeselectingWithBox { oldModel } ->
            oldModel.edges


getImage : NewModel -> JigsawImage
getImage model =
    case model of
        Identity { configuration } ->
            configuration.image

        Moving { configuration } ->
            configuration.image

        SelectingWithBox { configuration } ->
            configuration.image

        DeselectingWithBox { configuration } ->
            configuration.image
