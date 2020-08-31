module Main exposing (main)

import Browser
import Drag
import JigsawImage exposing (isPieceGroupInsideBox, isPointInsidePieceGroup)
import Keyboard exposing (Keyboard)
import List
import Model
    exposing
        ( Key(..)
        , Msg(..)
        , NewModel
        , generateModel
        , init
        )
import PieceGroup
import Point exposing (Point)
import Seeded exposing (Seeded(..))
import Set exposing (Set)
import Subscription exposing (subscriptions)
import UI
import View exposing (view)


main : Program () (Seeded NewModel) Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = Seeded.unwrap >> view
        , subscriptions = Seeded.unwrap >> subscriptions
        }



-- UPDATE


update : Msg -> Seeded NewModel -> ( Seeded NewModel, Cmd Msg )
update msg seededModel =
    ( case msg of
        Scramble ->
            seededModel
                |> Seeded.map (.configuration >> .image)
                |> Seeded.map generateModel
                |> Seeded.step

        KeyDown keyboard key ->
            seededModel
                |> Seeded.map (updateKeyChange keyboard key)

        MouseDown coordinate keyboard ->
            seededModel
                |> Seeded.map (updateMouseDown coordinate keyboard)

        MouseUp ->
            seededModel
                |> Seeded.map updateMouseUp

        MouseMove newPos ->
            seededModel
                |> Seeded.map (updateMoveMouse newPos)
    , Cmd.none
    )



{- this could be replaced from Set.Extra -}


sToggle : comparable -> Set comparable -> Set comparable
sToggle a set =
    if Set.member a set then
        Set.remove a set

    else
        Set.insert a set


updateKeyChange : Keyboard -> Maybe Key -> NewModel -> NewModel
updateKeyChange keyboard key model =
    case key of
        Just (Number x) ->
            if keyboard.ctrl then
                { model
                    | selected =
                        model.selected
                            |> List.map (\pg -> { pg | visibilityGroup = x })
                }

            else
                { model
                    | visibleGroups = sToggle x model.visibleGroups
                }

        _ ->
            model


updateMouseDown : Point -> Keyboard -> NewModel -> NewModel
updateMouseDown coordinate keyboard model =
    let
        { selected, unSelected, visibleGroups } =
            model

        { image } =
            model.configuration

        clickedPieceGroup =
            (unSelected ++ selected)
                |> List.filter (isPointInsidePieceGroup visibleGroups image coordinate)
                |> List.reverse
                |> List.head

        mode =
            if keyboard.shift then
                UI.Add

            else if keyboard.ctrl then
                UI.Remove

            else
                UI.Replace
    in
    case clickedPieceGroup of
        Nothing ->
            { model | ui = UI.Boxing mode (Drag.from coordinate) }

        Just pg ->
            let
                ui =
                    UI.Moving UI.Snap (Drag.from coordinate)
            in
            if List.member pg selected then
                { model | ui = ui }

            else if keyboard.shift then
                { model
                    | ui = ui
                    , selected = pg :: model.selected
                    , unSelected =
                        model.unSelected
                            |> List.filter ((/=) pg)
                }

            else
                { model
                    | ui = ui
                    , selected = [ pg ]
                    , unSelected =
                        (model.selected ++ model.unSelected)
                            |> List.filter ((/=) pg)
                }


updateMouseUp : NewModel -> NewModel
updateMouseUp model =
    let
        { selected, unSelected } =
            model
    in
    case model.ui of
        UI.Boxing mode drag ->
            let
                { x, y, w, h } =
                    Drag.getDimensions drag

                { image } =
                    model.configuration

                isWithin =
                    isPieceGroupInsideBox
                        image
                        (Point x y)
                        (Point (x + w) (y + h))
            in
            case mode of
                UI.Add ->
                    let
                        ( within, outside ) =
                            List.partition isWithin unSelected
                    in
                    { model
                        | ui = UI.WaitingForInput
                        , selected = selected ++ within
                        , unSelected = outside
                    }

                UI.Remove ->
                    let
                        ( within, outside ) =
                            List.partition isWithin selected
                    in
                    { model
                        | ui = UI.WaitingForInput
                        , selected = outside
                        , unSelected = unSelected ++ within
                    }

                UI.Replace ->
                    let
                        ( within, outside ) =
                            List.partition isWithin (selected ++ unSelected)
                    in
                    { model
                        | ui = UI.WaitingForInput
                        , selected = within
                        , unSelected = outside
                    }

        UI.Moving _ drag ->
            let
                move =
                    PieceGroup.move (Drag.distance drag)
            in
            case selected of
                pg :: [] ->
                    let
                        moved =
                            move pg

                        ( toMerge, newUnSelected ) =
                            List.partition
                                (PieceGroup.shouldBeMerged model.configuration.snapDistance moved)
                                model.unSelected
                    in
                    { model
                        | ui = UI.WaitingForInput
                        , selected = [ List.foldl PieceGroup.merge moved toMerge ]
                        , unSelected = newUnSelected
                    }

                _ ->
                    { model
                        | ui = UI.WaitingForInput
                        , selected =
                            selected
                                |> List.map move
                    }

        _ ->
            model


updateMoveMouse : Point -> NewModel -> NewModel
updateMoveMouse newPos model =
    case model.ui of
        UI.Moving mode drag ->
            { model
                | ui =
                    drag
                        |> Drag.to newPos
                        |> UI.Moving mode
            }

        UI.Boxing mode drag ->
            { model
                | ui =
                    drag
                        |> Drag.to newPos
                        |> UI.Boxing mode
            }

        _ ->
            model
