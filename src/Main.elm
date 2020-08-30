module Main exposing (main)

import Browser
import Browser.Events
import Decode exposing (pageCoordinates)
import Dict as D
import Drag
import JigsawImage exposing (isPieceGroupInsideBox, isPointInsidePieceGroup)
import Json.Decode
import List
import Model
    exposing
        ( Key(..)
        , Keyboard
        , Msg(..)
        , NewModel(..)
        , OldModel
        , Selected(..)
        , SelectionBox(..)
        , boxBottomRight
        , boxTopLeft
        , defaultPieceGroup
        , generateNewModel
        , getImage
        , init
        , toNewModel
        , toOldModel
        )
import PieceGroup exposing (PieceGroup)
import Point exposing (Point)
import Seeded exposing (Seeded(..))
import Set as S
import Util exposing (takeFirst)
import View exposing (view)


main : Program () (Seeded NewModel) Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = Seeded.unwrap >> view
        , subscriptions = subscriptions
        }



-- SUBSCRIPTIONS


subscriptions : Seeded NewModel -> Sub Msg
subscriptions newModel =
    let
        (Seeded _ model) =
            toOldModel newModel

        trackMouseMovement =
            if model.cursor /= Nothing then
                Decode.pageCoordinates
                    |> Json.Decode.map MouseMove
                    |> Browser.Events.onMouseMove

            else
                Sub.none

        trackMouseDown =
            Json.Decode.map2 MouseDown Decode.pageCoordinates Decode.keyboard
                |> Browser.Events.onMouseDown

        trackMouseUp =
            Browser.Events.onMouseUp (Json.Decode.succeed MouseUp)
    in
    Sub.batch
        [ trackMouseMovement
        , trackMouseDown
        , trackMouseUp
        , Decode.key
            |> Json.Decode.map (KeyChanged True)
            |> Browser.Events.onKeyDown
        , Decode.key
            |> Json.Decode.map (KeyChanged False)
            |> Browser.Events.onKeyUp
        ]



-- UPDATE


update : Msg -> Seeded NewModel -> ( Seeded NewModel, Cmd Msg )
update msg seededModel =
    let
        oldModel =
            toOldModel seededModel
    in
    case msg of
        Scramble ->
            ( seededModel
                |> Seeded.map getImage
                |> Seeded.map generateNewModel
                |> Seeded.step
            , Cmd.none
            )

        KeyChanged isDown key ->
            oldModel
                |> Seeded.map (updateKeyChange isDown key)
                |> Seeded.map (Tuple.mapFirst toNewModel)
                |> Seeded.embed

        MouseDown coordinate keyboard ->
            oldModel
                |> Seeded.map (updateMouseDown coordinate keyboard)
                |> Seeded.map (Tuple.mapFirst toNewModel)
                |> Seeded.embed

        MouseUp ->
            oldModel
                |> Seeded.map updateMouseUp
                |> Seeded.map (Tuple.mapFirst toNewModel)
                |> Seeded.embed

        MouseMove newPos ->
            seededModel
                |> Seeded.map (updateMoveMouse newPos)
                |> Seeded.embed



{- this could be replaced from Set.Extra -}


sToggle : comparable -> S.Set comparable -> S.Set comparable
sToggle a set =
    if S.member a set then
        S.remove a set

    else
        S.insert a set


updateKeyChange : Bool -> Key -> OldModel -> ( OldModel, Cmd msg )
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
                    ( model
                    , Cmd.none
                    )

        Control ->
            let
                newKeyboard keyboard =
                    { keyboard | ctrl = isDown }
            in
            ( { model | keyboard = newKeyboard model.keyboard }
            , Cmd.none
            )

        Shift ->
            let
                newKeyboard keyboard =
                    { keyboard | shift = isDown }
            in
            ( { model | keyboard = newKeyboard model.keyboard }
            , Cmd.none
            )

        Other ->
            ( model
            , Cmd.none
            )


updateMouseDown : Point -> Keyboard -> OldModel -> ( OldModel, Cmd Msg )
updateMouseDown coordinate keyboard model =
    let
        clickedPieceGroup =
            D.values model.pieceGroups
                |> List.filter (isPointInsidePieceGroup model.visibleGroups model.image coordinate)
                |> List.reverse
                |> List.head
                |> Maybe.withDefault defaultPieceGroup

        clickedOnBackground =
            clickedPieceGroup.id == -10

        newModel =
            if clickedOnBackground then
                startSelectionBox model coordinate keyboard

            else
                selectPieceGroup model clickedPieceGroup.id coordinate keyboard
    in
    ( newModel
    , Cmd.none
    )


updateMouseUp : OldModel -> ( OldModel, Cmd Msg )
updateMouseUp model =
    ( case model.selectionBox of
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
    , Cmd.none
    )


updateMoveMouse : Point -> NewModel -> ( NewModel, Cmd Msg )
updateMoveMouse newPos model =
    ( case model of
        Model.Moving data ->
            Model.Moving
                { data
                    | drag =
                        data.drag
                            |> Drag.to newPos
                }

        Model.SelectingWithBox data ->
            let
                oldModel =
                    data.oldModel

                box =
                    { staticCorner = data.drag |> Drag.getStart
                    , movingCorner = newPos
                    , selectedIds = S.empty
                    }
            in
            Model.SelectingWithBox
                { data
                    | drag = data.drag |> Drag.to newPos
                    , within =
                        oldModel.pieceGroups
                            |> D.values
                            |> List.filter
                                (\pg -> S.member pg.visibilityGroup oldModel.visibleGroups)
                            |> List.filter
                                (isPieceGroupInsideBox
                                    oldModel.image
                                    (boxTopLeft box)
                                    (boxBottomRight box)
                                )
                }

        Model.DeselectingWithBox data ->
            Model.DeselectingWithBox
                { data | drag = data.drag |> Drag.to newPos }

        Model.Identity data ->
            Model.Identity data
    , Cmd.none
    )


selectPieceGroup : OldModel -> Int -> Point -> Keyboard -> OldModel
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
            D.insert id clickedPieceGroup

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
        | cursor = Just coordinate
        , selected = currentSelection <| newPieceGroups model.pieceGroups
        , pieceGroups = newPieceGroups model.pieceGroups
    }


startSelectionBox : OldModel -> Point -> Keyboard -> OldModel
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


snapToNeighbour : OldModel -> PieceGroup -> D.Dict Int PieceGroup
snapToNeighbour model selected =
    let
        neighbourFromId : Int -> PieceGroup
        neighbourFromId id =
            Maybe.withDefault defaultPieceGroup <|
                D.get id model.pieceGroups

        isVisible : PieceGroup -> Bool
        isVisible pg =
            S.member pg.visibilityGroup model.visibleGroups

        visibleNeighbours =
            selected.neighbours
                |> S.toList
                |> List.map neighbourFromId
                |> List.filter isVisible

        distanceToSelected : List ( Float, PieceGroup )
        distanceToSelected =
            visibleNeighbours
                |> List.map (\x -> ( x, x ))
                |> List.map (Tuple.mapFirst (PieceGroup.distance selected))

        closeNeighbour : Maybe PieceGroup
        closeNeighbour =
            distanceToSelected
                |> takeFirst (Tuple.first >> (>=) model.snapDistance)
                |> Maybe.map Tuple.second
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
