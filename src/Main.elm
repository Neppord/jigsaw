module Main exposing (main)

import Browser
import Browser.Events
import Dict as D
import Edge exposing (generateEdgePoints)
import JigsawImage
    exposing
        ( JigsawImage
        , createPieceGroups
        , isPieceGroupInsideBox
        , isPointInsidePieceGroup
        , shufflePiecePositions
        )
import Json.Decode
import Model
    exposing
        ( Key(..)
        , Keyboard
        , Model
        , Msg(..)
        , Selected(..)
        , SelectionBox(..)
        , boxBottomRight
        , boxTopLeft
        , defaultPieceGroup
        )
import PieceGroup exposing (PieceGroup)
import Point exposing (Point)
import Random
import Random.List
import Set as S
import Util exposing (takeFirst)
import View exposing (view)


main : Program () Model Msg
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
        , Browser.Events.onKeyDown
            (Json.Decode.map
                (keyDecoder True)
                (Json.Decode.field "key" Json.Decode.string)
            )
        , Browser.Events.onKeyUp
            (Json.Decode.map
                (keyDecoder False)
                (Json.Decode.field "key" Json.Decode.string)
            )
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyChanged isDown key ->
            updateKeyChange isDown key model

        Scramble ->
            let
                newModel =
                    resetModel model.image model.seed
            in
            ( newModel, Cmd.none )

        MouseDown coordinate keyboard ->
            updateMouseDown coordinate keyboard model

        MouseUp ->
            updateMouseUp model

        MouseMove newPos ->
            updateMoveMouse newPos model



{- this could be replaced from Set.Extra -}


sToggle : comparable -> S.Set comparable -> S.Set comparable
sToggle a set =
    if S.member a set then
        S.remove a set

    else
        S.insert a set


updateKeyChange : Bool -> Key -> Model -> ( Model, Cmd msg )
updateKeyChange isDown key model =
    let
        assignVisibilityGroup visibilityGroup _ pg =
            if pg.isSelected && pg.visibilityGroup /= visibilityGroup then
                { pg | visibilityGroup = visibilityGroup, isSelected = False }

            else
                pg

        newPieceGroups visibilityGroup =
            D.map (assignVisibilityGroup visibilityGroup) model.pieceGroups
    in
    case key of
        Number x ->
            case ( model.keyboard.ctrl, isDown ) of
                ( True, True ) ->
                    ( { model | pieceGroups = newPieceGroups x }
                    , Cmd.none
                    )

                ( False, True ) ->
                    ( { model
                        | visibleGroups = sToggle x model.visibleGroups
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


updateMouseDown : Point -> Keyboard -> Model -> ( Model, Cmd Msg )
updateMouseDown coordinate keyboard model =
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
    ( newModel, Cmd.none )


updateMouseUp : Model -> ( Model, Cmd Msg )
updateMouseUp model =
    let
        newModel =
            case model.selectionBox of
                Normal _ ->
                    { model
                        | selectionBox = NullBox
                        , cursor = Nothing
                        , selected = currentSelection model.pieceGroups
                    }

                Inverted _ ->
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


updateMoveMouse : Point -> Model -> ( Model, Cmd Msg )
updateMoveMouse newPos model =
    case ( model.cursor, model.selectionBox ) of
        ( Nothing, _ ) ->
            ( model, Cmd.none )

        ( Just oldPos, NullBox ) ->
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

        ( Just _, Normal box ) ->
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

        ( Just _, Inverted box ) ->
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
        neighbourFromId : Int -> PieceGroup
        neighbourFromId id =
            Maybe.withDefault defaultPieceGroup <|
                D.get id model.pieceGroups

        distanceToSelected : List ( Float, PieceGroup )
        distanceToSelected =
            selected.neighbours
                |> S.toList
                |> List.map neighbourFromId
                |> List.map (\x -> (x, x))
                |> List.map (Tuple.mapFirst (PieceGroup.distance selected))

        smallEnough : ( Float, a ) -> Bool
        smallEnough ( dx, _ ) =
            dx < model.snapDistance

        closeNeighbour : Maybe PieceGroup
        closeNeighbour =
            distanceToSelected
                |> takeFirst smallEnough 
                |> Maybe.andThen
                    (\( _, neighbour ) ->
                        if S.member neighbour.visibilityGroup model.visibleGroups then
                            Just neighbour

                        else
                            Nothing
                    )
    in
    case closeNeighbour of
        Just neighbour ->
            let
                replace : Int -> Int -> S.Set Int -> S.Set Int
                replace wrong right neighbours =
                    if S.member wrong neighbours then
                        neighbours
                            |> S.remove wrong
                            |> S.insert right

                    else
                        neighbours

                replaceSelectedIdWithNeighbourId _ pg =
                    { pg
                        | neighbours =
                            pg.neighbours
                                |> replace selected.id neighbour.id
                    }
            in
            model.pieceGroups
                |> D.insert neighbour.id (PieceGroup.merge selected neighbour)
                |> D.remove selected.id
                |> D.map replaceSelectedIdWithNeighbourId

        Nothing ->
            model.pieceGroups


allSelectedPieceGroups : D.Dict Int PieceGroup -> D.Dict Int PieceGroup
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
