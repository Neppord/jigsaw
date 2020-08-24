module Model exposing
    ( Box
    , Key(..)
    , Keyboard
    , Model
    , Msg(..)
    , NewModel(..)
    , Selected(..)
    , SelectionBox(..), getImage, getEdges, getVisibilityGroups
    , boxBottomRight
    , boxTopLeft
    , defaultPieceGroup
    , init
    , resetModel
    , toNewModel
    , toOldModel
    )

import Dict
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
import Random.List
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


type alias Model =
    { cursor : Maybe Point
    , pieceGroups : Dict.Dict Int PieceGroup
    , selected : Selected
    , maxZLevel : Int
    , image : JigsawImage
    , width : Int
    , height : Int
    , snapDistance : Float
    , selectionBox : SelectionBox
    , seed : Random.Seed
    , edges : List (List Edge)
    , visibleGroups : S.Set Int
    , keyboard : Keyboard
    }


type NewModel
    = Identity
        { oldModel : Model
        , selected : List PieceGroup
        , unSelected : List PieceGroup
        }
    | Moving
        { oldModel : Model
        , start : Point
        , current : Point
        , selected : List PieceGroup
        , unSelected : List PieceGroup
        }
    | SelectingWithBox
        { oldModel : Model
        , start : Point
        , current : Point
        , within : List PieceGroup
        , alreadySelected : List PieceGroup
        , unSelected : List PieceGroup
        }
    | DeselectingWithBox
        { oldModel : Model
        , start : Point
        , current : Point
        , within : List PieceGroup
        , alreadySelected : List PieceGroup
        , unSelected : List PieceGroup
        }


toNewModel : Model -> NewModel
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
                , start = current
                , current = current
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
                , start = box.staticCorner
                , current = box.movingCorner
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
                , start = box.staticCorner
                , current = box.movingCorner
                , alreadySelected = alreadySelected
                , within = within
                , unSelected = notSelected
                }

        ( _, _ ) ->
            Identity
                { oldModel = oldModel
                , selected = selected
                , unSelected = unSelected
                }


toOldModel : NewModel -> Model
toOldModel newModel =
    case newModel of
        Identity { oldModel } ->
            oldModel

        Moving { oldModel, start, current, selected, unSelected } ->
            { oldModel
                | pieceGroups =
                    selected
                        |> List.map (PieceGroup.move (Point.sub current start))
                        |> List.append unSelected
                        |> List.map (\pg -> ( pg.id, pg ))
                        |> Dict.fromList
                , cursor = Just current
            }

        SelectingWithBox { oldModel, start, current, alreadySelected, within } ->
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
                    { staticCorner = start
                    , movingCorner = current
                    , selectedIds = alreadySelectedIds
                    }
            in
            { oldModel
                | selectionBox = Normal box
                , pieceGroups = updatedPieceGroups
            }

        DeselectingWithBox { oldModel, start, current, alreadySelected, within } ->
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
                    { staticCorner = start
                    , movingCorner = current
                    , selectedIds = alreadySelectedIds
                    }
            in
            { oldModel
                | selectionBox = Inverted box
                , pieceGroups = updatedPieceGroups
            }


init : () -> ( NewModel, Cmd Msg )
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

        model =
            resetModel image (Random.initialSeed 0)
    in
    ( model, Cmd.none )


resetModel : JigsawImage -> Random.Seed -> NewModel
resetModel image seed =
    let
        ( w, h ) =
            ( image.width, image.height )

        ( nx, ny ) =
            ( image.xpieces, image.ypieces )

        numberOfEdges =
            2 * nx * ny - nx - ny

        ( positions, seed1 ) =
            Random.step (shufflePiecePositions w h image) seed

        ( zlevels, seed2 ) =
            shuffleZLevels (nx * ny) seed1

        ( edgePoints, seed3 ) =
            Random.step (generateEdgePoints numberOfEdges) seed2
    in
    toNewModel
        { cursor = Nothing
        , pieceGroups = createPieceGroups image positions zlevels
        , selected = NullSelection
        , maxZLevel = nx * ny
        , image = image
        , width = w
        , height = h
        , snapDistance = 30.0
        , selectionBox = NullBox
        , seed = seed3
        , edges =
            List.range 0 (image.xpieces * image.ypieces - 1)
                |> List.map (\id -> Edge.pieceEdges image.xpieces image.ypieces id edgePoints)
        , visibleGroups = S.fromList [ -1 ]
        , keyboard = { shift = False, ctrl = False }
        }


shuffleZLevels : Int -> Random.Seed -> ( List Int, Random.Seed )
shuffleZLevels n seed =
    Random.step (Random.List.shuffle <| List.range 0 (n - 1)) seed


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
    , zlevel = -1
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
        Identity { oldModel } ->
            oldModel.image

        Moving { oldModel } ->
            oldModel.image

        SelectingWithBox { oldModel } ->
            oldModel.image

        DeselectingWithBox { oldModel } ->
            oldModel.image