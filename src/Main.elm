module Main exposing (main)

import Browser
import Browser.Events
import Dict as D
import Edge exposing (generateEdgePoints)
import JigsawImage
    exposing
        ( JigsawImage
        , PieceGroup
        , createPieceGroups
        , isPieceGroupInsideBox
        , isPointInsidePieceGroup
        , shufflePiecePositions
        )
import Json.Decode
import Model exposing (..)
import Point exposing (Point)
import Random
import Random.List
import Set as S
import Util exposing (takeFirst)
import View exposing (view)


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL
-- INIT


init : () -> ( Model, Cmd Msg )
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


resetModel : JigsawImage -> Random.Seed -> Model
resetModel image seed =
    let
        ( w, h ) =
            ( 1800, 1100 )

        ( nx, ny ) =
            ( image.xpieces, image.ypieces )

        numberOfEdges =
            2 * nx * ny - nx - ny

        ( positions, seed1 ) =
            shufflePiecePositions w h image seed

        ( zlevels, seed2 ) =
            shuffleZLevels (nx * ny) seed1

        ( edgePoints, seed3 ) =
            Random.step (generateEdgePoints numberOfEdges) seed2
    in
    { cursor = Nothing
    , pieceGroups = createPieceGroups image positions zlevels
    , selected = NullSelection
    , maxZLevel = nx * ny
    , image = image
    , width = w
    , height = h
    , snapDistance = 30.0
    , selectionBox = NullBox
    , debug = "Nothing to see here..."
    , seed = seed3
    , edgePoints = edgePoints
    , visibleGroups = S.fromList [ -1 ]
    , keyboard = { shift = False, ctrl = False }
    }


shuffleZLevels : Int -> Random.Seed -> ( List Int, Random.Seed )
shuffleZLevels n seed =
    Random.step (Random.List.shuffle <| List.range 0 (n - 1)) seed



-- SUBSCRIPTIONS


keyDecoder : Bool -> String -> Msg
keyDecoder isDown key =
    case key of
        "0" ->
            KeyChanged isDown (Number 0)

        "1" ->
            KeyChanged isDown (Number 1)

        "2" ->
            KeyChanged isDown (Number 2)

        "3" ->
            KeyChanged isDown (Number 3)

        "4" ->
            KeyChanged isDown (Number 4)

        "5" ->
            KeyChanged isDown (Number 5)

        "6" ->
            KeyChanged isDown (Number 6)

        "7" ->
            KeyChanged isDown (Number 7)

        "8" ->
            KeyChanged isDown (Number 8)

        "9" ->
            KeyChanged isDown (Number 9)

        "Control" ->
            KeyChanged isDown Control

        "Shift" ->
            KeyChanged isDown Shift

        _ ->
            KeyChanged isDown Other


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        trackMouseMovement =
            if model.cursor /= Nothing then
                Browser.Events.onMouseMove <|
                    Json.Decode.map2 (\x y -> MouseMove (Point x y))
                        (Json.Decode.field "pageX" Json.Decode.int)
                        (Json.Decode.field "pageY" Json.Decode.int)

            else
                Sub.none

        trackMouseDown =
            Browser.Events.onMouseDown <|
                Json.Decode.map4 (\x y shift ctrl -> MouseDown (Point x y) { shift = shift, ctrl = ctrl })
                    (Json.Decode.field "pageX" Json.Decode.int)
                    (Json.Decode.field "pageY" Json.Decode.int)
                    (Json.Decode.field "shiftKey" Json.Decode.bool)
                    (Json.Decode.field "ctrlKey" Json.Decode.bool)

        trackMouseUp =
            Browser.Events.onMouseUp (Json.Decode.succeed MouseUp)
    in
    Sub.batch
        [ trackMouseMovement
        , trackMouseDown
        , trackMouseUp
        , Browser.Events.onKeyDown (Json.Decode.map (keyDecoder True) (Json.Decode.field "key" Json.Decode.string))
        , Browser.Events.onKeyUp (Json.Decode.map (keyDecoder False) (Json.Decode.field "key" Json.Decode.string))
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyChanged isDown key ->
            let
                assignVisibilityGroup visibilityGroup _ pg =
                    if pg.isSelected && pg.visibilityGroup /= visibilityGroup then
                        { pg | visibilityGroup = visibilityGroup, isSelected = False }

                    else
                        pg

                newPieceGroups visibilityGroup =
                    D.map (assignVisibilityGroup visibilityGroup) model.pieceGroups

                toggleVisibilityOf group number =
                    if S.member number group then
                        S.remove number group

                    else
                        S.insert number group
            in
            case key of
                Number x ->
                    case ( model.keyboard.ctrl, isDown ) of
                        ( True, True ) ->
                            ( { model | pieceGroups = newPieceGroups x }, Cmd.none )

                        ( False, True ) ->
                            ( { model
                                | visibleGroups = toggleVisibilityOf model.visibleGroups x
                                , debug =
                                    S.toList (toggleVisibilityOf model.visibleGroups x)
                                        |> List.map String.fromInt
                                        |> List.intersperse ", "
                                        |> String.concat
                              }
                            , Cmd.none
                            )

                        ( _, False ) ->
                            ( model, Cmd.none )

                Control ->
                    let
                        newKeyboard keyboard =
                            { keyboard | ctrl = isDown }
                    in
                    ( { model | keyboard = newKeyboard model.keyboard }, Cmd.none )

                Shift ->
                    let
                        newKeyboard keyboard =
                            { keyboard | shift = isDown }
                    in
                    ( { model | keyboard = newKeyboard model.keyboard }, Cmd.none )

                Other ->
                    ( model, Cmd.none )

        Scramble ->
            let
                newModel =
                    resetModel model.image model.seed
            in
            ( newModel, Cmd.none )

        MouseDown coordinate keyboard ->
            let
                clickedPieceGroup =
                    D.values model.pieceGroups
                        |> List.filter (isPointInsidePieceGroup model.visibleGroups model.image coordinate)
                        |> List.foldl
                            (\a b ->
                                if a.zlevel > b.zlevel then
                                    a

                                else
                                    b
                            )
                            defaultPieceGroup

                clickedOnBackground =
                    clickedPieceGroup.id == -10

                newModel =
                    if clickedOnBackground then
                        startSelectionBox model coordinate keyboard

                    else
                        selectPieceGroup model clickedPieceGroup.id coordinate keyboard
            in
            ( { newModel | debug = String.fromInt clickedPieceGroup.id }, Cmd.none )

        MouseUp ->
            let
                newModel =
                    case model.selectionBox of
                        Normal box ->
                            { model
                                | selectionBox = NullBox
                                , cursor = Nothing
                                , selected = currentSelection model.pieceGroups
                            }

                        Inverted box ->
                            { model
                                | selectionBox = NullBox
                                , cursor = Nothing
                                , selected = currentSelection model.pieceGroups
                            }

                        NullBox ->
                            case model.selected of
                                Multiple ->
                                    { model | cursor = Nothing }

                                NullSelection ->
                                    { model | cursor = Nothing }

                                Single id ->
                                    { model
                                        | cursor = Nothing
                                        , selected = NullSelection
                                        , pieceGroups =
                                            D.get id model.pieceGroups
                                                |> Maybe.withDefault defaultPieceGroup
                                                |> snapToNeighbour model
                                    }
            in
            ( newModel, Cmd.none )

        MouseMove newPos ->
            case model.cursor of
                Nothing ->
                    ( model, Cmd.none )

                Just oldPos ->
                    case model.selectionBox of
                        NullBox ->
                            let
                                movePieceGroup : Int -> PieceGroup -> PieceGroup
                                movePieceGroup _ pg =
                                    if pg.isSelected then
                                        { pg | position = Point.add pg.position <| Point.sub newPos oldPos }

                                    else
                                        pg

                                updatedModel =
                                    { model
                                        | cursor = Just newPos
                                        , pieceGroups = D.map movePieceGroup model.pieceGroups
                                    }
                            in
                            ( updatedModel, Cmd.none )

                        Normal box ->
                            let
                                tl =
                                    boxTopLeft box

                                br =
                                    boxBottomRight box

                                selectPiece _ pg =
                                    let
                                        isVisible =
                                            S.member pg.visibilityGroup model.visibleGroups

                                        originallySelected =
                                            S.member pg.id box.selectedIds

                                        insideBoxNow =
                                            isPieceGroupInsideBox model.image tl br pg

                                        newSelectionStatus =
                                            if isVisible then
                                                if originallySelected && insideBoxNow then
                                                    True

                                                else if originallySelected && not insideBoxNow then
                                                    True

                                                else if not originallySelected && insideBoxNow then
                                                    True

                                                else
                                                    False

                                            else
                                                False
                                    in
                                    { pg | isSelected = newSelectionStatus }

                                updatedPieceGroups =
                                    D.map selectPiece model.pieceGroups
                            in
                            ( { model
                                | selectionBox = Normal { box | movingCorner = newPos }
                                , pieceGroups = updatedPieceGroups
                              }
                            , Cmd.none
                            )

                        Inverted box ->
                            let
                                tl =
                                    boxTopLeft box

                                br =
                                    boxBottomRight box

                                selectPiece _ pg =
                                    let
                                        isVisible =
                                            S.member pg.visibilityGroup model.visibleGroups

                                        originallySelected =
                                            S.member pg.id box.selectedIds

                                        insideBoxNow =
                                            isPieceGroupInsideBox model.image tl br pg

                                        newSelectionStatus =
                                            if isVisible then
                                                if originallySelected && insideBoxNow then
                                                    False

                                                else if originallySelected && not insideBoxNow then
                                                    True

                                                else if not originallySelected && insideBoxNow then
                                                    True

                                                else
                                                    False

                                            else
                                                False
                                    in
                                    { pg | isSelected = newSelectionStatus }

                                updatedPieceGroups =
                                    D.map selectPiece model.pieceGroups
                            in
                            ( { model
                                | selectionBox = Inverted { box | movingCorner = newPos }
                                , pieceGroups = updatedPieceGroups
                              }
                            , Cmd.none
                            )


selectPieceGroup : Model -> Int -> Point -> Keyboard -> Model
selectPieceGroup model id coordinate keyboard =
    let
        clickedPieceGroup =
            D.get id model.pieceGroups
                |> Maybe.withDefault defaultPieceGroup

        wasSelectedBefore =
            clickedPieceGroup.isSelected

        shouldStartDragging =
            wasSelectedBefore && model.selected == Multiple

        fixZlevels =
            D.insert id { clickedPieceGroup | zlevel = model.maxZLevel }

        selectClickedPieceGroup =
            D.insert id { clickedPieceGroup | isSelected = True }

        invertClickedPieceGroup =
            D.insert id { clickedPieceGroup | isSelected = not clickedPieceGroup.isSelected }

        deselectAllOther =
            D.map (\key pg -> { pg | isSelected = key == id })

        newPieceGroups =
            if keyboard.ctrl then
                invertClickedPieceGroup

            else if keyboard.shift then
                selectClickedPieceGroup << fixZlevels

            else if shouldStartDragging then
                fixZlevels

            else
                deselectAllOther << fixZlevels
    in
    { model
        | maxZLevel = model.maxZLevel + 1
        , cursor = Just coordinate
        , selected = currentSelection <| newPieceGroups model.pieceGroups
        , pieceGroups = newPieceGroups model.pieceGroups
    }


startSelectionBox : Model -> Point -> Keyboard -> Model
startSelectionBox model coordinate keyboard =
    let
        ids =
            allSelectedPieceGroups model.pieceGroups
                |> D.keys
                |> S.fromList
    in
    if keyboard.ctrl then
        { model
            | cursor = Just coordinate
            , selectionBox =
                Inverted
                    { staticCorner = coordinate
                    , movingCorner = coordinate
                    , selectedIds = ids
                    }
        }

    else if keyboard.shift then
        { model
            | cursor = Just coordinate
            , selectionBox =
                Normal
                    { staticCorner = coordinate
                    , movingCorner = coordinate
                    , selectedIds = ids
                    }
        }

    else
        { model
            | cursor = Just coordinate
            , selected = NullSelection
            , pieceGroups = D.map (\_ pg -> { pg | isSelected = False }) model.pieceGroups
            , selectionBox =
                Normal
                    { staticCorner = coordinate
                    , movingCorner = coordinate
                    , selectedIds = S.empty
                    }
        }


snapToNeighbour : Model -> PieceGroup -> D.Dict Int PieceGroup
snapToNeighbour model selected =
    let
        neighbourDistance : PieceGroup -> PieceGroup -> ( Float, PieceGroup )
        neighbourDistance selectedPiece neighbour =
            ( Point.dist selectedPiece.position neighbour.position
            , neighbour
            )

        neighbourFromId : Int -> PieceGroup
        neighbourFromId id =
            Maybe.withDefault defaultPieceGroup <|
                D.get id model.pieceGroups

        distanceToSelected : List ( Float, PieceGroup )
        distanceToSelected =
            List.map (neighbourDistance selected << neighbourFromId) (S.toList selected.neighbours)

        smallEnough : ( Float, a ) -> Bool
        smallEnough ( distance, _ ) =
            distance < model.snapDistance

        closeNeighbour : Maybe PieceGroup
        closeNeighbour =
            case takeFirst smallEnough distanceToSelected of
                Nothing ->
                    Nothing

                Just ( _, neighbour ) ->
                    if S.member neighbour.visibilityGroup model.visibleGroups then
                        Just neighbour

                    else
                        Nothing

        merge : PieceGroup -> PieceGroup -> PieceGroup
        merge a b =
            let
                newMembers =
                    b.members ++ a.members

                newNeighbours =
                    S.diff (S.union b.neighbours a.neighbours) (S.fromList newMembers)
            in
            { b
                | isSelected = False
                , members = newMembers
                , neighbours = newNeighbours
                , zlevel = a.zlevel
            }
    in
    case closeNeighbour of
        Just neighbour ->
            let
                fixNeighbours : S.Set Int -> Int -> Int -> S.Set Int
                fixNeighbours neighbours wrong right =
                    if S.member wrong neighbours then
                        S.insert right <| S.remove wrong neighbours

                    else
                        neighbours

                replaceSelectedIdWithNeighbourId _ pg =
                    { pg | neighbours = fixNeighbours pg.neighbours selected.id neighbour.id }
            in
            merge selected neighbour
                |> Util.flip (D.insert neighbour.id) model.pieceGroups
                |> D.remove selected.id
                |> D.map replaceSelectedIdWithNeighbourId

        Nothing ->
            model.pieceGroups


allSelectedPieceGroups pieceGroups =
    D.filter (\_ pg -> pg.isSelected) pieceGroups


currentSelection : D.Dict Int PieceGroup -> Selected
currentSelection pieceGroups =
    case D.keys <| allSelectedPieceGroups pieceGroups of
        [] ->
            NullSelection

        id :: [] ->
            Single id

        _ ->
            Multiple
