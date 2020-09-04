module Main exposing (main)

import Browser
import DB
import Drag
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

        ChangeImageUrl url ->
            let
                updateModel model =
                    { model | configuration = updateConfiguration model.configuration }

                updateConfiguration configuration =
                    { configuration | image = updateImage configuration.image }

                updateImage image =
                    { image | path = url }
            in
            seededModel
                |> Seeded.map updateModel
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
                    | db =
                        model.db
                            |> DB.modifySelected
                                (\pg ->
                                    { pg
                                        | visibilityGroup = x
                                        , isSelected = True
                                    }
                                )
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
        { db, visibleGroups } =
            model

        { image } =
            model.configuration

        clickedPieceGroup =
            db
                |> DB.findBy
                    (PieceGroup.isPointInsidePieceGroup visibleGroups image coordinate)
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
            if pg.isSelected then
                { model | ui = ui }

            else if keyboard.shift then
                { model
                    | ui = ui
                    , db = model.db |> DB.modify pg.id PieceGroup.select
                }

            else
                { model
                    | ui = ui
                    , db =
                        model.db
                            |> DB.modifySelected PieceGroup.deselect
                            |> DB.modify pg.id PieceGroup.select
                }


updateMouseUp : NewModel -> NewModel
updateMouseUp model =
    case model.ui of
        UI.Boxing mode drag ->
            let
                { x, y, w, h } =
                    Drag.getDimensions drag

                { image } =
                    model.configuration

                topLeft =
                    Point x y

                bottomRight =
                    Point (x + w) (y + h)

                isWithin =
                    PieceGroup.isPieceGroupInsideBox image topLeft bottomRight

                shouldBeSelected pg =
                    Set.member pg.visibilityGroup model.visibleGroups
                        && isWithin pg

                db =
                    case mode of
                        UI.Add ->
                            model.db
                                |> DB.modifyBy
                                    (\pg -> shouldBeSelected pg || pg.isSelected)
                                    PieceGroup.select

                        UI.Remove ->
                            model.db
                                |> DB.modifyBy
                                    (\pg -> shouldBeSelected pg || not pg.isSelected)
                                    PieceGroup.deselect

                        UI.Replace ->
                            model.db
                                |> DB.map
                                    (\pg ->
                                        if pg |> shouldBeSelected then
                                            PieceGroup.select pg

                                        else
                                            PieceGroup.deselect pg
                                    )
            in
            { model | ui = UI.WaitingForInput, db = db }

        UI.Moving _ drag ->
            let
                move =
                    PieceGroup.move (Drag.distance drag)
            in
            case model.db |> DB.getSelected of
                pg :: [] ->
                    let
                        moved =
                            move pg

                        shouldBeMerged x =
                            PieceGroup.shouldBeMerged model.configuration.snapDistance moved x
                                || x.id
                                == pg.id
                    in
                    { model
                        | ui = UI.WaitingForInput
                        , db =
                            model.db
                                |> DB.modify pg.id move
                                |> DB.aggregateBy
                                    shouldBeMerged
                                    PieceGroup.merge
                    }

                _ ->
                    { model
                        | ui = UI.WaitingForInput
                        , db = DB.modifySelected move model.db
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
